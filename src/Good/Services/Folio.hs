{-# LANGUAGE QuasiQuotes #-}

module Good.Services.Folio where

import Good.Prelude

import System.IO (hPutStrLn)

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Data.Aeson (Value (..), FromJSON, parseJSON, withObject, (.:), ToJSON, toJSON, encode, object, (.=))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?))
import qualified Clay as C
import qualified Clay.Size as C.S
import qualified Clay.Render as C.R
import qualified Clay.Border as C.B
import qualified Clay.Font as C.F

import Language.Javascript.JMacro

import qualified Network.WebSockets as WS

import qualified Good.Architecture.Scraper as Scraper
import Good.Architecture.Input
import Good.Architecture.Inputs.FSRead
import Good.Architecture.Output
import Good.Architecture.Outputs.FSWrite
import Good.Interfaces.Web

boolrow :: Text -> Text -> (FolioState -> Bool) -> FolioState -> H.Html
boolrow name remote access is = H.tr $ mconcat
  [ H.td $ H.toHtml name
  , H.td $ H.select ! A.class_ "boolrow" ! A.id (H.textValue remote) $ mconcat
    [ (if access is then H.option ! A.value "true" ! A.selected "selected" else H.option ! A.value "true") "On"
    , (if access is then H.option ! A.value "false" else H.option ! A.value "false" ! A.selected "selected") "Off"
    ]
  ]

sensorrow :: Text -> Text -> (FolioState -> Text) -> FolioState -> H.Html
sensorrow name remote access is = H.tr $ mconcat
  [ H.td $ H.toHtml name
  , H.td $ H.span ! A.class_ "sensorrow" ! A.id (H.textValue remote) $ H.toHtml $ access is
  ]

api :: IO (Serving IO ())
api = do
  clients <- newMVar (setFromList [])
  pure $ do
    handling (Get "/folio/state") $ do
      is <- liftIO getState
      pure . Markup . H.docTypeHtml $ mconcat
        [ H.head $ mconcat
          [ H.title "Folio: A Personalized Hydroponics System"
          , H.style . H.toHtml $ C.renderWith C.R.compact [] stylesheet
          ]
        , H.body $ mconcat
          [ H.h1 "Folio: A Personalized Hydroponics System"
          , H.table $ mconcat
            [ H.tr $ mconcat [ H.td $ H.span ! A.class_ "column-header" $ "Key" , H.td $ H.span ! A.class_ "column-header" $ "Value"]
            , boolrow "Light" "light" light is
            , boolrow "Valve" "valve" valve is
            , sensorrow "Water" "water" water is
            , sensorrow "Light Level" "lightlevel" lightlevel is
            ]
          , H.script ! A.src "https://code.jquery.com/jquery-3.3.1.min.js" ! H.customAttribute "integrity" "sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8=" ! H.customAttribute "crossorigin" "anonymous" $ ""
          , H.script . H.toHtml . tshow $ renderJs script
          ]
        ]
    handling (Socket "/folio/socket") $ \conn -> do
      WS.forkPingThread conn 30
      name <- toText <$> nextRandom
      hPutStrLn stderr . toSL $ "client joined: " <> name
      modifyMVar_ clients (pure . insertSet (Client name conn))
      catch
        (forever $ do d <- WS.receiveData conn :: IO Text
                      (FolioMessage key v) <- Scraper.fromJSON d
                      is <- getState
                      let s = case v of
                                Left value -> case key of "light" -> is { light = value }
                                                          "valve" -> is { valve = value }
                                                          _ -> is
                                Right value -> case key of "water" -> is { water = value }
                                                           "lightlevel" -> is { lightlevel = value }
                                                           _ -> is
                      hPutStrLn stderr . toSL $ mconcat ["set ", key, " = ", toSL $ show v]
                      cs <- liftIO $ readMVar clients
                      liftIO . broadcast cs $ \(Client n c) -> do
                        hPutStrLn stderr . toSL $ "sending to client: " <> n
                        WS.sendTextData c . encode $ FolioMessage key v
                      outputting (FSWriteConfig "store/folio") $ putJSON (FSWrite "state") s)
        ((\_ -> modifyMVar_ clients (\ucs -> do hPutStrLn stderr . toSL $ "client left: " <> name
                                                pure $ deleteSet (Client name conn) ucs)) :: SomeException -> IO ())

stylesheet :: C.Css
stylesheet = mconcat
  [ C.body ? mconcat [ C.marginLeft $ C.S.pct 10
                     , C.maxWidth $ C.S.pct 80
                     , C.lineHeight $ C.S.pct 160
                     , C.fontSize $ C.S.px 18
                     , C.color $ C.rgb 0x44 0x44 0x44
                     , C.paddingTop $ C.S.px 10
                     , C.paddingBottom $ C.S.px 10
                     , C.fontFamily ["Helvetica"] [C.F.sansSerif]
                     ]
  , (C.h1 <> C.h2 <> C.h3) ? mconcat [ C.lineHeight (C.S.pct 120)
                                     , C.textAlign C.center
                                     ]
  , C.table ? mconcat [ C.width $ C.S.pct 100
                      , C.borderCollapse C.collapse
                      ]
  , C.td ? mconcat [ C.border C.B.solid (C.S.px 2) (C.rgb 0x88 0x88 0x88)
                   , C.fontSize $ C.S.px 32
                   , C.paddingTop $ C.S.px 5
                   , C.paddingBottom $ C.S.px 5
                   , C.paddingLeft $ C.S.px 5
                   , C.paddingRight $ C.S.px 5
                   ]
  , ".column-header" ? mconcat [ C.fontWeight C.F.bold
                               ]
  ]

script :: JStat
script = [jmacro|
  var socket = new WebSocket('ws://' + window.location.host + '/folio/socket');

  socket.addEventListener('message', function(event) {
    msg = JSON.parse(event.data);
    if ($('#' + msg.key).attr("class") === "boolrow") {
      $('#' + msg.key).val(msg.value.toString());
    } else {
      $('#' + msg.key).html(msg.value.toString());
    }
  });

  $('.boolrow').each(function () {
    $(this).change(function (_) {
      var k = $(this).attr('id');
      var val = $(this).val() === "true";
      socket.send(JSON.stringify({ key: k, value: val }));
    });
  });
|]

data Client = Client Text WS.Connection
instance Eq Client where (Client x _) == (Client y _) = x == y
instance Ord Client where compare (Client x _) (Client y _) = compare x y

broadcast :: Set Client -> (Client -> IO ()) -> IO ()
broadcast s f = omapM_ f s

newtype FolioError = FolioError Text deriving Show
instance Exception FolioError

data FolioState = FolioState { light :: Bool
                             , valve :: Bool
                             , water :: Text
                             , lightlevel :: Text
                             } deriving (Show, Eq, Generic)
instance FromJSON FolioState where
instance ToJSON FolioState where

data FolioMessage = FolioMessage Text (Either Bool Text)
                  deriving (Show, Eq)
instance FromJSON FolioMessage where
  parseJSON = withObject "foliomessage" $ \o -> do
    key <- o .: "key"
    case lookup "value" o of
      Just (Bool value) -> pure $ FolioMessage key (Left value)
      Just (String value) -> pure $ FolioMessage key (Right value)
      _ -> fail "Invalid message"
instance ToJSON FolioMessage where
  toJSON (FolioMessage key (Left value)) = object ["key" .= key, "value" .= value]
  toJSON (FolioMessage key (Right value)) = object ["key" .= key, "value" .= value]

getState :: IO FolioState
getState = catch
  (inputting (FSReadConfig "store/folio") $ getJSON (FSRead "state"))
  ((\_ -> let s = FolioState { light = False
                             , valve = False
                             , water = "N/A"
                             , lightlevel = "N/A"
                             }
          in do outputting (FSWriteConfig "store/folio") $ putJSON (FSWrite "state") s
                pure s) :: (SomeException -> IO FolioState))
