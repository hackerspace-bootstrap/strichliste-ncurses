{-# Language OverloadedStrings, DataKinds, TypeOperators, LambdaCase #-}

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import qualified Brick.Widgets.Center as B
import Control.Arrow ((&&&))
import Control.Concurrent (Chan, newChan, writeChan, forkIO)
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
  | PurchaseSuccessful Transaction
  | PurchaseFailed String

data UIState
  = UserMenu Text.Text -- ^ current filter
             (Trie.Trie User) -- ^ all users
             (B.List User) -- ^ currently displayed users
  | TransactionMenu User -- ^ selected user
                    Centi -- ^ user's balance
                    (B.List Centi) -- ^ possible transactions
                    (B.List Transaction) -- ^ past transactions
  | Processing User -- ^ User currently purchasing
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
    UserMenu prefix _ users ->
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
    UserMenu prefix users usersL -> case unsafeToVtyEvent e of
       Vty.EvKey Vty.KEsc _ ->
         B.halt uiState
       Vty.EvKey Vty.KEnter _ ->
         case B.listSelectedElement usersL of
              Nothing -> B.continue uiState
              Just (_, u) -> toEventM uiState (getTransactionMenu u) >>= B.continue
       Vty.EvKey (Vty.KChar c) [] ->
         filterUsers users $ prefix `Text.snoc` c
       Vty.EvKey Vty.KBS [] ->
         filterUsers users $ if Text.null prefix then prefix else Text.init prefix
       ev ->
         B.handleEvent ev usersL >>= B.continue . UserMenu prefix users
    TransactionMenu u balance amounts transactions -> case unsafeToVtyEvent e of
       Vty.EvKey Vty.KEsc _ ->
         toEventM uiState getUserMenu >>= B.continue
       Vty.EvKey Vty.KEnter _ ->
         case B.listSelectedElement amounts of
              Nothing -> B.continue uiState
              Just (_, a) -> do
                liftIO $ purchase u a chan
                B.continue $ Processing u uiState
       ev -> do
         newList <- B.handleEvent ev amounts
         B.continue $ TransactionMenu u balance newList transactions
    Processing u prev -> case e of
       VtyEvent _ -> B.continue uiState -- TODO: ability to abort action
       PurchaseSuccessful _trans -> toEventM uiState (getTransactionMenu u) >>= B.continue
       PurchaseFailed err -> B.continue (Error err prev)
    Error _ prev -> B.continue prev
  where
    filterUsers users p = B.continue $ UserMenu p users $ matchingUsers p users

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
    actionList = B.list "Actions" (V.fromList [-0.5, -1, -1.5, -2.0]) 1

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
mkUserMenu page = UserMenu "" usersT $ B.list "Users" usersL 1
  where
    (usersT, usersL) = indexUsers page

-- | Build a Trie and a sorted list of users from a page of users
indexUsers :: Page User -> (Trie.Trie User, V.Vector User)
indexUsers = (id &&& V.fromList . Trie.elems) . toTrie . pageEntries

-- | Build a Trie from a list of users
toTrie :: V.Vector User -> Trie.Trie User
toTrie = Trie.fromList . V.toList . V.map (Text.encodeUtf16LE . userName &&& id)

purchase :: MonadIO io => User -> Centi -> Chan MyEvent -> io ()
purchase (User _ uid _ _) amount chan = liftIO $ void $ forkIO $ do
    result <- eitherToEvent <$> runEitherT (postTransaction uid (Value amount))
    writeChan chan result
  where
    eitherToEvent (Left err) = PurchaseFailed (show err)
    eitherToEvent (Right trans) = PurchaseSuccessful trans

toEventM :: MonadIO io => UIState -> ServantT UIState -> io UIState
toEventM prev servantState = liftIO $ runEitherT servantState >>= \case
    Right st -> return st
    Left err -> return $ Error (show err) prev

emptyState :: UIState
emptyState = UserMenu "" Trie.empty $ B.list "Users" V.empty 1

main :: IO ()
main = do
    chan <- newChan
    users <- toEventM emptyState $ getUserMenu
    void $ B.customMain (Vty.mkVty mempty) chan (app chan) users
