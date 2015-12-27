{-# Language OverloadedStrings, DataKinds, TypeOperators, LambdaCase,
             ScopedTypeVariables #-}

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import qualified Brick.Widgets.Center as B
import Control.Arrow ((&&&))
import Control.Concurrent (Chan, newChan, writeChan, forkIO, ThreadId)
import Control.Exception (handle, throwTo, mask, Exception, SomeException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Aeson ( FromJSON, parseJSON, ToJSON, toJSON, (.:), (.:?), (.=)
                  , withObject, object
                  )
import Data.Fixed
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Trie as Trie
import Data.Typeable (Typeable)
import Formatting
import qualified Graphics.Vty as Vty
import Servant.API
import Servant.Client

type ServantT = EitherT ServantError IO

data Page a = Page
  { pageOverallCount :: Int
  , pageLimit :: Maybe Int
  , pageOffset :: Maybe Int
  , pageEntries :: V.Vector a
  }

instance FromJSON a => FromJSON (Page a) where
  parseJSON = withObject "page" $ \o ->
    Page <$> o .: "overallCount" <*> o .:? "limit" <*> o .:? "offset" <*> o .: "entries"


type Date = Text.Text
type ID = Int

data User = User
  { userName :: Text.Text
  , userID :: ID
  , userBalance :: Centi
  , userLastTransaction :: Maybe Date
  }

instance FromJSON User where
  parseJSON = withObject "user" $ \o ->
    User <$> o .: "name" <*> o .: "id" <*> o .: "balance" <*> o .:? "lastTransaction"


data Transaction = Transaction
  { taValue :: Centi
  , taCreateDate :: Date
  , taID :: ID
  , taUserID :: ID
  }

instance FromJSON Transaction where
  parseJSON = withObject "transaction" $ \o ->
    Transaction <$> o .: "value" <*> o .: "createDate" <*> o .: "id" <*> o .: "userId"


newtype Value = Value { getValue :: Centi }

instance ToJSON Value where
  toJSON (Value amount) = object [ "value" .= amount ]


type UserID = Capture "userId" ID
type Strichliste = "user" :> Get '[JSON] (Page User)
              :<|> "user" :> UserID :> Get '[JSON] User
              :<|> "user" :> UserID :> "transaction"
                :> Get '[JSON] (Page Transaction)
              :<|> "user" :> UserID :> "transaction"
                :> ReqBody '[JSON] Value
                :> Post '[JSON] Transaction

strichliste :: Proxy Strichliste
strichliste = Proxy

getUsers :: ServantT (Page User)
getUser :: ID -> ServantT User
getUserTransactions :: ID -> ServantT (Page Transaction)
postTransaction :: ID -> Value -> ServantT Transaction
getUsers :<|> getUser :<|> getUserTransactions :<|> postTransaction =
    client strichliste host
  where
    host = BaseUrl Https "demo-api.strichliste.org" 443

data MyEvent
  = VtyEvent Vty.Event
  | PurchaseSuccessful UIState
  | PurchaseFailed String

data FilterList a = FL
  { filterPrefix :: Text.Text
  , filterAll :: Trie.Trie a
  , filterUpdate :: Text.Text -> Trie.Trie a -> B.List a
  , filterCurrent :: B.List a
  }

instance B.HandleEvent (FilterList a) where
  handleEvent ev (FL fPrefix fAll fUpdate fCurrent) = case ev of
       Vty.EvKey (Vty.KChar c) [] ->
         filterUsers $ fPrefix `Text.snoc` c
       Vty.EvKey Vty.KBS [] ->
         filterUsers $ if Text.null fPrefix then fPrefix else Text.init fPrefix
       _ ->
         FL fPrefix fAll fUpdate <$> B.handleEvent ev fCurrent
    where
      filterUsers p = return $ FL p fAll fUpdate (fUpdate p fAll)

data UIState
  = UserMenu (FilterList User)
  | TransactionMenu User -- ^ selected user
                    Centi -- ^ user's balance
                    (B.List Centi) -- ^ possible transactions
                    (B.List Transaction) -- ^ past transactions
  | Processing ThreadId -- ^ Thread doing the work
               UIState -- ^ Previous state
  | Error String -- ^ Error description
          UIState -- ^ Previous state

app :: Chan MyEvent -> B.App UIState MyEvent
app chan = B.App
  { B.appDraw = drawUI
  , B.appStartEvent = return
  , B.appHandleEvent = appEvent chan
  , B.appAttrMap = const theMap
  , B.appLiftVtyEvent = VtyEvent
  , B.appChooseCursor = B.showFirstCursor
  }


drawUI :: UIState -> [B.Widget]
drawUI uiState =
  case uiState of
    UserMenu (FL prefix _ _ users) ->
      let box = B.borderWithLabel (B.str "Select user") $ B.renderList users drawUserListElement
          filterBox = B.borderWithLabel (B.str "Filter") $ B.hCenter $ B.txt prefix
          ui = B.vBox $ B.hCenter box : if Text.null prefix then [] else [ filterBox ]
      in [ui]
    TransactionMenu _ balance amounts transactions ->
      let menubox = B.borderWithLabel (B.str "Select amounts") $ B.renderList amounts drawActionListElement
          menu = B.vCenter $ B.hCenter menubox
          balanceW = B.hCenter $ B.txt $ sformat shown balance
          transactionW = B.renderList transactions drawTransactioListElement
          ui = B.hBox
                [ menu
                , B.borderWithLabel (B.str "User info") $ B.vBox
                    [ balanceW
                    , B.hBorderWithLabel (B.str "Past transactions")
                    , transactionW
                    ]
                ]
      in [ui]
    Processing _ prev ->
      let w = B.hCenter $ B.border $ B.str "Processing..."
      in w : drawUI prev
    Error err prev ->
      let errorW = B.hCenter
                 $ B.withAttr errorAttr
                 $ B.borderWithLabel (B.str "An error occured")
                 $ B.str err
      in errorW : drawUI prev


appEvent :: Chan MyEvent -> UIState -> MyEvent -> B.EventM (B.Next UIState)
appEvent chan uiState e =
  case uiState of
    UserMenu fl -> case unsafeToVtyEvent e of
       Vty.EvKey Vty.KEsc _ ->
         B.halt uiState
       Vty.EvKey Vty.KEnter _ ->
         case B.listSelectedElement (filterCurrent fl) of
              Nothing -> B.continue uiState
              Just (_, u) -> toEventM uiState (getTransactionMenu u) >>= B.continue
       ev -> B.handleEvent ev fl >>= B.continue . UserMenu
    TransactionMenu u balance amounts transactions -> case unsafeToVtyEvent e of
       Vty.EvKey Vty.KEsc _ ->
         toEventM uiState getUserMenu >>= B.continue
       Vty.EvKey Vty.KEnter _ ->
         case B.listSelectedElement amounts of
              Nothing -> B.continue uiState
              Just (_, a) -> do
                tid <- liftIO $ purchase u a chan
                B.continue $ Processing tid uiState
       ev -> do
         newList <- B.handleEvent ev amounts
         B.continue $ TransactionMenu u balance newList transactions
    Processing tid prev -> case e of
       VtyEvent (Vty.EvKey Vty.KEsc _) ->
         liftIO (throwTo tid Abort) >> B.continue uiState
       VtyEvent _ -> B.continue uiState
       PurchaseSuccessful newState -> B.continue newState
       PurchaseFailed err -> B.continue (Error err prev)
    Error _ prev -> B.continue prev -- TODO: try to redraw

unsafeToVtyEvent :: MyEvent -> Vty.Event
unsafeToVtyEvent (VtyEvent e) = e
unsafeToVtyEvent _ = error "Invalid event received!"

errorAttr :: B.AttrName
errorAttr = B.attrName "errorMsg"

theMap :: B.AttrMap
theMap = B.attrMap Vty.defAttr
  [ (B.listAttr,            Vty.white `B.on` Vty.black)
  , (B.listSelectedAttr,    Vty.black `B.on` Vty.white)
  , (errorAttr,             B.fg Vty.red)
  ]


getTransactionMenu :: User -> ServantT UIState
getTransactionMenu u = do
    u' <- getUser $ userID u
    TransactionMenu u' (userBalance u') actionList <$> getTransactions u'
  where
    actionList = B.list "Actions" (V.fromList [2, 1.5, 1, 0.5, -0.5, -1, -1.5, -2.0]) 1

getTransactions :: User -> ServantT (B.List Transaction)
getTransactions (User _ uid _ _) = do
    pastTrans <- getUserTransactions uid
    return $ B.list "Transactions" (pageEntries pastTrans) 1

drawActionListElement :: Bool -> Centi -> B.Widget
drawActionListElement _ amount = B.hCenter $ B.txt $ sformat shown amount

drawTransactioListElement :: Bool -> Transaction -> B.Widget
drawTransactioListElement _ (Transaction v cd _ _) =
  B.txt $ sformat (stext % ": " % shown % "€") cd v

matchingUsers :: Text.Text -> Trie.Trie User -> B.List User
matchingUsers prefix users = B.list "Users" (V.fromList matching) 1
  where
    matching = Trie.elems $ Trie.submap (Text.encodeUtf16LE prefix) users

drawUserListElement :: Bool -> User -> B.Widget
drawUserListElement _ u = B.hCenter $ B.txt $ userName u

getUserMenu :: ServantT UIState
getUserMenu = mkUserMenu <$> getUsers

mkUserMenu :: Page User -> UIState
mkUserMenu page = UserMenu $ FL "" usersT matchingUsers $ B.list "Users" usersL 1
  where
    (usersT, usersL) = indexUsers page

-- | Build a Trie and a sorted list of users from a page of users
indexUsers :: Page User -> (Trie.Trie User, V.Vector User)
indexUsers = (id &&& V.fromList . Trie.elems) . toTrie . pageEntries

-- | Build a Trie from a list of users
toTrie :: V.Vector User -> Trie.Trie User
toTrie = Trie.fromList . V.toList . V.map (Text.encodeUtf16LE . userName &&& id)

data Abort = Abort deriving (Show, Typeable)

instance Exception Abort

purchase :: MonadIO io => User -> Centi -> Chan MyEvent -> io ThreadId
purchase u@(User _ uid _ _) amount chan = liftIO $ mask $ \restore -> forkIO $
    handle (\(_ :: Abort) -> writeChan chan (PurchaseFailed "Aborted")) $
    handle (\(e :: SomeException) -> writeChan chan (PurchaseFailed (show e))) $
    restore $
      runEitherT (postTransaction uid (Value amount) >> getTransactionMenu u)
      >>= writeChan chan . eitherToEvent
  where
    eitherToEvent (Left err) = PurchaseFailed (show err)
    eitherToEvent (Right uiState) = PurchaseSuccessful uiState

toEventM :: MonadIO io => UIState -> ServantT UIState -> io UIState
toEventM prev servantState = liftIO $ runEitherT servantState >>= \case
    Right st -> return st
    Left err -> return $ Error (show err) prev

emptyState :: UIState
emptyState = UserMenu $ FL "" Trie.empty matchingUsers $ B.list "Users" V.empty 1

main :: IO ()
main = do
    chan <- newChan
    users <- toEventM emptyState $ getUserMenu
    void $ B.customMain (Vty.mkVty mempty) chan (app chan) users
