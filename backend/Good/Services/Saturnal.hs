module Good.Services.Saturnal where

import Good.Prelude

import Data.Aeson (eitherDecode)
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

boardStore :: Text
boardStore = "store/saturnal/board"

turnStore :: Text
turnStore = "store/saturnal/turn"
 
api :: Serving IO ()
api = do
  handling (Get "/saturnal") . pure . Markup . H.docTypeHtml $ mconcat
    [ H.head $ mconcat
      [ H.title "Saturnal"
      , H.style . H.toHtml $ C.renderWith C.R.compact [] stylesheet
      ]
    , H.body $ mconcat
      [ H.canvas ! A.id "canvas" ! A.class_ "hidden" ! A.style "background-color: white; background-size: 20px 20px; background-image: linear-gradient(to right, lightgrey 1px, transparent 1px), linear-gradient(to bottom, lightgrey 1px, transparent 1px);" $ ""
      , H.div ! A.id "topbar" $ mconcat
        [ H.button ! A.id "alpha" $ "α: " <> (H.span ! A.id "alphaval" $ "0")
        , H.button ! A.id "beta" $ "β: " <> (H.span ! A.id "betaval" $ "0")
        , H.button ! A.id "gamma" $ "γ: " <> (H.span ! A.id "gammaval" $ "0")
        , H.button ! A.id "delta" $ "δ: " <> (H.span ! A.id "deltaval" $ "0")
        , H.button ! A.id "hex" $ H.b "⬡"
        , H.button ! A.id "endturn" $ "End Turn"
        ]
      , H.div ! A.id "popup" ! A.class_ "hidden" $ mconcat
        [ H.h3 ! A.id "popuptitle" $ ""
        , H.div ! A.id "popupbody" $ ""
        ]
      , H.div ! A.id "menu" ! A.class_ "hidden" $ mconcat
        [ H.div ! A.id "loginmenu" $ mconcat
          [ H.input ! A.type_ "text" ! A.id "username" ! A.placeholder "Username"
          , H.input ! A.type_ "password" ! A.id "password" ! A.placeholder "Password"
          , H.button ! A.id "login" $ "Login"
          ]
        , H.div ! A.id "loggedinmenu" $ mconcat
          [ "Logged in as "
          , H.span ! A.id "loggedinas" $ ""
          , H.div ! A.id "joingamemenu" $ mconcat
            [ H.input ! A.type_ "text" ! A.id "gameid" ! A.placeholder "Game ID"
            , H.button ! A.id "joingame" $ "Join"
            ]
          , H.div ! A.id "joinedgamemenu" $ mconcat
            [ "Game "
            , H.span ! A.id "joinedgame" $ ""
            ]
          ]
        , H.hr
        , H.h4 "Saturnal"
        , H.p $ mconcat ["Copyright 2018 ", H.a ! A.href "https://chame.co" $ "Samuel Breese"]
        , H.p $ mconcat ["This program is free software: you can redistribute it and/or modify it under the terms of the ", H.a ! A.href "https://www.gnu.org/licenses/gpl.html" $ "GNU General Public License", " as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version."]
        , H.p "This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details."
        ]
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
    uuid <- liftIO (toText <$> nextRandom)
    setBoard uuid board
    pure $ Plaintext uuid
  handling (Get "/saturnal/board/:board") . withBoard $ \_ board _ -> pure $ JSON board
  handling (Put "/saturnal/board/:board/invite") . withBoard $ \uuid board _ -> do
    player <- param "player"
    if player `elem` boardPlayers board
      then pure ()
      else setBoard uuid $ board { boardPlayers = player:boardPlayers board }
    pure $ Plaintext "Invite success"
  handling (Post "/saturnal/board/:board/turn") . withBoard $ \uuid board player ->
    param "turn"
    >>= throwLeft (DecodeError . toSL) . eitherDecode . toSL
    >>= addTurn uuid board player
    >>= (\case False -> pure ()
               True -> resolveTurn uuid board >>= setBoard uuid)
    >> pure (Plaintext "Successfully submitted turn")

stylesheet :: C.Css
stylesheet = mconcat
  [ "*" ? mconcat
    [ C.margin (C.S.px 0) (C.S.px 0) (C.S.px 0) (C.S.px 0)
    , C.fontFamily [] [C.monospace]
    ]
  , "html" ? C.height (C.S.pct 100)
  , "body" ? mconcat
    [ C.height (C.S.pct 100)
    , C.backgroundColor C.lightgrey
    ]
  , "#canvas" ? mconcat
    [ C.width (C.S.pct 100)
    , C.height (C.S.pct 100)
    , C.position C.absolute
    , C.transition "opacity" (C.sec 0.25) C.linear (C.sec 0)
    ]
  , "#topbar" ? mconcat
    [ C.width (C.S.pct 100)
    , C.height (C.S.px 32)
    , C.position C.absolute
    , C.backgroundColor C.grey
    ]
  , "#alpha" ? C.float C.floatLeft
  , "#beta" ? C.float C.floatLeft <> C.marginLeft (C.S.px 0)
  , "#gamma" ? C.float C.floatLeft <> C.marginLeft (C.S.px 0)
  , "#delta" ? C.float C.floatLeft <> C.marginLeft (C.S.px 0)
  , "#hex" ? C.float C.floatRight 
  , "#endturn" ? C.float C.floatRight <> C.marginRight (C.S.px 0)
  , "#popup" ? mconcat
    [ C.position C.absolute
    , C.left (C.S.px 5)
    , C.top (C.S.px 37)
    , C.width (C.S.px 250)
    , C.height (C.S.px 400)
    , C.backgroundColor C.grey
    , C.color C.white
    , C.padding (C.S.px 10) (C.S.px 10) (C.S.px 10) (C.S.px 10)
    , C.transition "opacity" (C.sec 0.25) C.linear (C.sec 0)
    ]
  , "#menu" ? mconcat
    [ C.position C.absolute
    , C.right (C.S.px 5)
    , C.top (C.S.px 37)
    , C.width (C.S.px 250)
    , C.height (C.S.px 400)
    , C.backgroundColor C.grey
    , C.color C.white
    , C.padding (C.S.px 10) (C.S.px 10) (C.S.px 10) (C.S.px 10)
    , C.transition "opacity" (C.sec 0.25) C.linear (C.sec 0)
    ]
  , "#loggedinmenu" ? C.display C.none
  , "#joinedgamemenu" ? C.display C.none
  , ".hidden" ? C.opacity 0
  , C.a ? mconcat
    [ C.color C.white
    , C.fontWeight C.bold
    ]
  , C.button <> C.input ? mconcat
    [ C.boxShadow [C.none]
    , C.background (C.none :: C.BackgroundImage)
    , C.backgroundColor C.white
    , C.borderStyle C.solid
    , C.borderColor C.black
    , C.marginTop (C.S.px 5)
    , C.marginBottom (C.S.px 5)
    , C.marginLeft (C.S.px 5)
    , C.marginRight (C.S.px 5)
    ]
  , C.hr ? mconcat
    [ C.boxShadow [C.none]
    , C.color C.white
    , C.marginTop (C.S.px 5)
    , C.marginBottom (C.S.px 5)
    ]
  ]

withAuth :: (MonadIO m, MonadCatch m) => (Text -> Handling m a) -> Handling m a
withAuth body = do
  (player, token) <- catch
    ((,) <$> cookie "player" <*> cookie "token")
    (\(_ :: WebError) -> throwM $ WebError forbidden403 "Unauthorized")
  lastToken <- getToken player
  if token /= lastToken
    then throwM $ WebError forbidden403 "Unauthorized"
    else body player

withBoard :: (MonadIO m, MonadCatch m) => (Text -> Board -> Text -> Handling m a) -> Handling m a
withBoard body = do
  uuid <- param "board"
  board <- getBoard uuid
  withAuth $ \player -> if player `elem` boardPlayers board
    then body uuid board player
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
getBoard uuid = inputting (FSReadConfig boardStore) $ getJSON (FSRead uuid)

setBoard :: (MonadIO m, MonadCatch m) => Text -> Board -> m ()
setBoard uuid board = outputting (FSWriteConfig boardStore) $ putJSON (FSWrite uuid) board

resetTurn :: (MonadIO m, MonadCatch m) => Text -> m ()
resetTurn uuid = outputting (FSWriteConfig turnStore) $ putRaw (FSWrite uuid) "[]"

addTurn :: (MonadIO m, MonadCatch m) => Text -> Board -> Text -> Turn -> m Bool
addTurn uuid board player turn
  | player == turnPlayer turn = do
      (turns :: [Turn]) <- catch (inputting (FSReadConfig turnStore) $ getJSON (FSRead uuid))
                         (\(_ :: SomeException) -> pure [])
      outputting (FSWriteConfig turnStore) . putJSON (FSWrite uuid) $ turn:turns
      let players = turnPlayer <$> turn:turns
      pure . all (`elem` players) $ boardPlayers board
  | otherwise = throwM $ WebError forbidden403 "Not authenticated to control player"

resolveTurn :: (MonadIO m, MonadCatch m) => Text -> Board -> m Board
resolveTurn uuid board = do
  ts :: [Turn] <- inputting (FSReadConfig turnStore) $ getJSON (FSRead uuid)
  pure $ (foldr processTurn board $ sortOn (Down . turnBid) ts) { boardTurn = succ (boardTurn board) }
  where processTurn :: Turn -> Board -> Board
        processTurn t b = foldr (processMove (turnPlayer t)) b $ turnMoves t
        processMove :: Text -> Move -> Board -> Board
        processMove _ _ b = b
