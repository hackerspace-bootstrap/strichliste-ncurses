{-# Language OverloadedStrings #-}

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Widgets.Core
  ( Widget
  , txt
  , vBox
  , hBox
  )
import Brick.Util (on)
import Control.Arrow ((&&&))
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Fixed
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Trie as Trie
import Formatting
import qualified Graphics.Vty as V
import qualified Network.Wreq as W

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


data UIState
  = UserMenu Text.Text -- ^ current filter
             (Trie.Trie User) -- ^ all users
             (L.List User) -- ^ currently displayed users
  | TransactionMenu User -- ^ selected user
                    Centi -- ^ user's balance
                    (L.List Centi) -- ^ possible transactions
                    (L.List Transaction) -- ^ past transactions


app :: String -> M.App UIState V.Event
app api = M.App { M.appDraw = drawUI
                , M.appStartEvent = return
                , M.appHandleEvent = appEvent api
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


appEvent :: String -> UIState -> V.Event -> M.EventM (M.Next UIState)
appEvent api s@(UserMenu prefix users usersL) e =
  case e of
       V.EvKey V.KEsc _ -> M.halt s
       V.EvKey V.KEnter _ ->
         case L.listSelectedElement usersL of
              Nothing -> M.continue s
              Just (_, u) -> mkTransactionMenu api u >>= M.continue
       V.EvKey (V.KChar c) [] -> filterUsers $ prefix `Text.snoc` c
       V.EvKey V.KBS [] -> filterUsers $ if Text.null prefix
                                           then prefix
                                           else Text.init prefix
       ev -> M.continue $ UserMenu prefix users $ T.handleEvent ev usersL
  where
    filterUsers p = M.continue $ UserMenu p users $ matchingUsers p users
appEvent api s@(TransactionMenu u balance amounts transactions) e =
  case e of
       V.EvKey V.KEsc _ -> mkUserMenu api >>= M.continue
       V.EvKey V.KEnter _ ->
         case L.listSelectedElement amounts of
              Nothing -> M.continue s
              Just (_, a) -> do
                purchase api u a
                s' <- mkTransactionMenu api u
                M.continue s'
       ev -> M.continue $ TransactionMenu u balance (T.handleEvent ev amounts) transactions


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr,            V.white `on` V.black)
  , (L.listSelectedAttr,    V.black `on` V.white)
  ]


mkTransactionMenu :: MonadIO io => String -> User -> io UIState
mkTransactionMenu api u = liftIO $ do
    transactionList <- getTransactions api u
    return $ TransactionMenu u (userBalance u) actionList transactionList
  where
    actionList = L.list "Actions" drawActionListElement
                        [-0.5, -1, -1.5, -2.0]

getTransactions :: MonadIO io => String -> User -> io (L.List Transaction)
getTransactions api (User _ uid _ _) = liftIO $ do
  resp <- W.get $ userTransactions api uid
  page <- either (error . ("Unable to retrieve user transaction: " ++)) return $
    eitherDecode $ resp ^. W.responseBody
  return $ L.list "Transactions" drawTransactioListElement $ pageEntries page

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

mkUserMenu :: MonadIO io => String -> io UIState
mkUserMenu api = liftIO $ do
  resp <- W.get $ api <> "/user"
  (usersT, usersL) <- either (error . ("Unable to retrieve user list: " ++)) return $ do
    fmap indexUsers $ eitherDecode $ resp ^. W.responseBody
  return $ UserMenu "" usersT $ L.list "Users" drawUserListElement usersL

-- | Build a Trie and a sorted list of users from a page of users
indexUsers :: Page User -> (Trie.Trie User, [User])
indexUsers = (id &&& Trie.elems) . toTrie . pageEntries

-- | Build a Trie from a list of users
toTrie :: [User] -> Trie.Trie User
toTrie = Trie.fromList . map (Text.encodeUtf16LE . userName &&& id)

purchase :: MonadIO io => String -> User -> Centi -> io ()
purchase api (User _ uid _ _) amount =
    liftIO $ void $ W.post (userTransactions api uid) $ object [ "value" .= amount]

userTransactions :: String -> Int -> String
userTransactions = formatToString (string % "/user/" % int % "/transaction")


main :: IO ()
main = do
  let api = "https://demo-api.strichliste.org"
  users <- mkUserMenu api
  void $ M.defaultMain (app api) users
