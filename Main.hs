{-# Language OverloadedStrings, DataKinds, TypeOperators, LambdaCase #-}

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Widgets.Core (Widget, txt, str, vBox, hBox)
import Brick.Util (on)
import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Aeson ( FromJSON, parseJSON, ToJSON, toJSON, (.:), (.:?), (.=)
                  , withObject, object
                  )
import Data.Fixed
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Trie as Trie
import Formatting
import qualified Graphics.Vty as V
import Servant.API
import Servant.Client

type ServantT = EitherT ServantError IO

data Page a = Page
  { pageOverallCount :: Int
  , pageLimit :: Maybe Int
  , pageOffset :: Maybe Int
  , pageEntries :: [a]
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


data UIState
  = UserMenu Text.Text -- ^ current filter
             (Trie.Trie User) -- ^ all users
             (L.List User) -- ^ currently displayed users
  | TransactionMenu User -- ^ selected user
                    Centi -- ^ user's balance
                    (L.List Centi) -- ^ possible transactions
                    (L.List Transaction) -- ^ past transactions
  | Error String


app :: M.App UIState V.Event
app = M.App { M.appDraw = drawUI
            , M.appStartEvent = return
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const theMap
            , M.appLiftVtyEvent = id
            , M.appChooseCursor = M.showFirstCursor
            }


drawUI :: UIState -> [Widget]
drawUI (UserMenu prefix _ users) = [ui]
  where
    box = B.borderWithLabel "Select user" $ L.renderList users
    filterBox = B.borderWithLabel "Filter" $ C.hCenter $ txt prefix
    ui = vBox $ C.hCenter box : if Text.null prefix then [] else [ filterBox ]
drawUI (TransactionMenu _ balance amounts transactions) = [ui]
  where
    menubox = B.borderWithLabel "Select amounts" $ L.renderList amounts
    menu = C.vCenter $ C.hCenter menubox
    balanceW = C.hCenter $ txt $ sformat shown balance
    transactionW = L.renderList transactions
    ui = hBox [ menu
              , B.borderWithLabel "User info" $ vBox
                  [ balanceW
                  , B.hBorderWithLabel "Past transactions"
                  , transactionW
                  ]
              ]
drawUI (Error err) = [str $ show err]


appEvent :: UIState -> V.Event -> M.EventM (M.Next UIState)
appEvent s@(UserMenu prefix users usersL) e =
  case e of
       V.EvKey V.KEsc _ -> M.halt s
       V.EvKey V.KEnter _ ->
         case L.listSelectedElement usersL of
              Nothing -> M.continue s
              Just (_, u) -> toEventM (getTransactionMenu u) >>= M.continue
       V.EvKey (V.KChar c) [] -> filterUsers $ prefix `Text.snoc` c
       V.EvKey V.KBS [] -> filterUsers $ if Text.null prefix
                                           then prefix
                                           else Text.init prefix
       ev -> M.continue $ UserMenu prefix users $ T.handleEvent ev usersL
  where
    filterUsers p = M.continue $ UserMenu p users $ matchingUsers p users
appEvent s@(TransactionMenu u balance amounts transactions) e =
  case e of
       V.EvKey V.KEsc _ -> toEventM getUserMenu >>= M.continue
       V.EvKey V.KEnter _ ->
         case L.listSelectedElement amounts of
              Nothing -> M.continue s
              Just (_, a) ->
                toEventM (purchase u a >>= getTransactionMenu) >>= M.continue
       ev -> M.continue $ TransactionMenu u balance (T.handleEvent ev amounts) transactions
appEvent s@(Error _) e =
  case e of
       V.EvKey V.KEsc _ -> M.halt s
       _ -> toEventM getUserMenu >>= M.continue


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,            V.white `on` V.black)
  , (L.listSelectedAttr,    V.black `on` V.white)
  ]


getTransactionMenu :: User -> ServantT UIState
getTransactionMenu u =
    TransactionMenu u (userBalance u) actionList <$> getTransactions u
  where
    actionList = L.list "Actions" drawActionListElement
                        [-0.5, -1, -1.5, -2.0]

getTransactions :: User -> ServantT (L.List Transaction)
getTransactions (User _ uid _ _) =
  L.list "Transactions" drawTransactioListElement . pageEntries <$> getUserTransactions uid

drawActionListElement :: Bool -> Centi -> Widget
drawActionListElement _ amount = C.hCenter $ txt $ sformat shown amount

drawTransactioListElement :: Bool -> Transaction -> Widget
drawTransactioListElement _ (Transaction v cd _ _) =
  txt $ sformat (stext % ": " % shown % "€") cd v

matchingUsers :: Text.Text -> Trie.Trie User -> L.List User
matchingUsers prefix users = L.list "Users" drawUserListElement matching
  where
    matching = Trie.elems $ Trie.submap (Text.encodeUtf16LE prefix) users

drawUserListElement :: Bool -> User -> Widget
drawUserListElement _ u = C.hCenter $ txt $ userName u

getUserMenu :: ServantT UIState
getUserMenu = mkUserMenu <$> getUsers

mkUserMenu :: Page User -> UIState
mkUserMenu page = UserMenu "" usersT $ L.list "Users" drawUserListElement usersL
  where
    (usersT, usersL) = indexUsers page

-- | Build a Trie and a sorted list of users from a page of users
indexUsers :: Page User -> (Trie.Trie User, [User])
indexUsers = (id &&& Trie.elems) . toTrie . pageEntries

-- | Build a Trie from a list of users
toTrie :: [User] -> Trie.Trie User
toTrie = Trie.fromList . map (Text.encodeUtf16LE . userName &&& id)

purchase :: User -> Centi -> ServantT User
purchase (User _ uid _ _) amount = do
  _ <- postTransaction uid (Value amount)
  getUser uid

toEventM :: MonadIO io => ServantT UIState -> io UIState
toEventM servantState = liftIO $ runEitherT servantState >>= \case
    Right st -> return st
    Left err -> return $ Error $ show err


main :: IO ()
main = do
  users <- toEventM $ getUserMenu
  void $ M.defaultMain app users
