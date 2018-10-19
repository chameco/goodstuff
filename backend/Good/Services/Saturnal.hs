module Good.Services.Saturnal where

import Good.Prelude

import Data.Aeson (ToJSON)
import qualified Data.ByteString.Base64 as Base64
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?))
import qualified Clay as C
import qualified Clay.Size as C.S
import qualified Clay.Render as C.R

import Crypto.Random
import Crypto.KDF.BCrypt

import qualified Database.SQLite.Simple as DB

import Good.Architecture.Input
import Good.Architecture.Inputs.FSRead
import Good.Architecture.Output
import Good.Architecture.Outputs.FSWrite
import Good.Interfaces.Web
import Good.Services.Saturnal.Types

db :: MonadIO m => (DB.Connection -> IO a) -> m a
db body = liftIO . DB.withConnection "db/saturnal.sqlite3" $ \conn -> DB.withTransaction conn (body conn)

store :: Text
store = "store/saturnal"

api :: Serving IO ()
api = do
  handling (Get "/saturnal") . pure . Markup . H.docTypeHtml $ mconcat
    [ H.head $ mconcat
      [ H.title "Saturnal"
      , H.style . H.toHtml $ C.renderWith C.R.compact [] stylesheet
      ]
    , H.body $ mconcat
      [ H.canvas ! A.id "canvas" $ ""
      , H.script ! A.src "/saturnal/main.js" $ ""
      ]
    ]
  handling (Get "/saturnal/main.js") . pure . FS (FSReadConfig "assets/saturnal") $ FSRead "js/main.js"
  handling (Post "/saturnal/register") $ do
    player <- param "player"
    pass <- param "pass"
    register player pass
    pure $ Plaintext "Registration success"
  handling (Post "/saturnal/login") $ do
    player <- param "player"
    pass <- param "pass"
    token <- login player pass
    setCookie ("player", player)
    setCookie ("token", token)
    pure $ Plaintext "Login success"
  handling (Post "/saturnal/board") . withAuth $ \player -> do
    let board = Board { boardCells = [[]]
                      , boardWidth = 0
                      , boardHeight = 0
                      , boardTurn = 0
                      , boardPlayers = [player]
                      }
    uuid <- toText <$> nextRandom
    setBoard uuid board
    pure $ Plaintext uuid
  handling (Get "/saturnal/board/:board") . withBoard $ \board _ -> pure board

stylesheet :: C.Css
stylesheet = mconcat
  [ "#canvas" ? mconcat
    [ C.width $ C.S.pct 100
    , C.height $ C.S.pct 100
    ]
  ]

withAuth :: (MonadIO m, MonadCatch m) => (Text -> m a) -> Handling m a
withAuth body = do
  (player, token) <- catch
    ((,) <$> cookie "player" <*> cookie "token")
    (\(_ :: WebError) -> throwM $ WebError forbidden403 "Unauthorized")
  lastToken <- getToken player
  if token /= lastToken
    then throwM $ WebError forbidden403 "Unauthorized"
    else lift (body player)

withBoard :: (MonadIO m, MonadCatch m, ToJSON a) => (Board -> Text -> m a) -> Handling m Response
withBoard body = do
  board <- param "board" >>= getBoard
  withAuth $ \player -> if player `elem` boardPlayers board
    then JSON <$> body board player
    else throwM $ WebError forbidden403 "Unauthorized"

ensureTables :: DB.Connection -> IO ()
ensureTables conn = DB.execute_ conn "create table if not exists players (player text primary key, hash blob, token text)"

register :: (MonadIO m, MonadThrow m) => Text -> Text -> m ()
register player pass = do
  (hashed :: ByteString) <- liftIO $ hashPassword 12 (toSL pass :: ByteString)
  db $ \conn -> do
    ensureTables conn
    res <- DB.query conn "select player from players where player=?" (DB.Only player)
    case res of
      [] -> void $ DB.execute conn "insert into players (player, hash) values (?, ?)" (player, hashed)
      ((_ :: [Text]):_) -> throwM $ WebError unprocessableEntity422 "User already exists"
  void $ newToken player

login :: (MonadIO m, MonadThrow m) => Text -> Text -> m Text
login player pass = do
  res <- db $ \conn ->
    DB.query conn "select hash from players where player=?" (DB.Only player)
  case res of
    ((hashed:_):_) ->
      if validatePassword (toSL pass :: ByteString) (hashed :: ByteString)
      then newToken player
      else throwM $ WebError unprocessableEntity422 "Authentication failure"
    _ -> throwM $ WebError unprocessableEntity422 "Authentication failure"

newToken :: MonadIO m => Text -> m Text
newToken player = do
  token <- toSL . Base64.encode <$> liftIO (getRandomBytes 128)
  void . db $ \conn -> do
    ensureTables conn
    DB.execute conn "update players set token=? where player=?" (token, player)
  pure token

getToken :: (MonadIO m, MonadThrow m) => Text -> m Text
getToken player = do
  res <- db $ \conn ->
    DB.query conn "select token from players where player=?" (DB.Only player)
  case res of
    ((token:_):_) -> pure token
    _ -> throwM $ WebError forbidden403 "User does not exist"

getBoard :: (MonadIO m, MonadCatch m) => Text -> m Board
getBoard bid = inputting (FSReadConfig store) $ getJSON (FSRead bid)

setBoard :: (MonadIO m, MonadCatch m) => Text -> Board -> m ()
setBoard bid board = outputting (FSWriteConfig store) $ putJSON (FSWrite bid) board

resolveTurns :: [Turn] -> Board -> Board
resolveTurns = error "Unimplemented"
