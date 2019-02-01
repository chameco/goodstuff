module Good.Services.Coal.Snapshot where

import Good.Prelude

import Data.Time.Clock

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?))
import qualified Clay as C
import qualified Clay.Size as C.S
import qualified Clay.Render as C.R

import Good.Architecture.Input
import Good.Architecture.Inputs.FSRead
import Good.Architecture.Output
import Good.Architecture.Outputs.FSWrite
import Good.Interfaces.Web
import Good.Services.Coal.Snapshot.Types
import Good.Services.Coal.Snapshot.Info

snapshotStore :: Text
snapshotStore = "store/coal/snapshot/snapshot"

infoStore :: Text
infoStore = "store/coal/snapshot/info"

api :: Text -> Text -> Serving IO ()
api botuser botpass = do
  handling (Get "/coal/snapshot") . pure . Markup . H.docTypeHtml $ mconcat
    [ H.head $ mconcat
      [ H.title "Snapshot"
      , H.style . H.toHtml $ C.renderWith C.R.compact [] stylesheet
      ]
    , H.body $ mconcat
      [ H.div ! A.id "topbar" $ mconcat
        [ H.button ! A.id "upload" $ "Upload"
        , H.button ! A.id "profile" $ "Profile"
        , H.button ! A.id "iotm" $ "IotM"
        , H.button ! A.id "skills" $ "Skills"
        , H.button ! A.id "snapshot" $ "📷"
        , H.input ! A.type_ "text" ! A.id "playerid" ! A.placeholder "Player ID"
        ]
      , H.div ! A.id "uploadtab" ! A.class_ "tab" $ mconcat
        [ H.textarea ! A.style "resize: none" ! A.id "uploadtext" ! A.placeholder "Paste snapshot here..." $ ""
        , H.input ! A.type_ "text" ! A.id "uploadplayerid" ! A.placeholder "Player ID"
        , H.button ! A.id "uploadsubmit" $ "Submit!"
        ]
      , H.div ! A.id "profiletab" ! A.class_ "tab" $ mconcat
        [ "profile"
        ]
      , H.div ! A.id "iotmtab" ! A.class_ "tab" $ mconcat
        [ "iotms"
        ]
      , H.div ! A.id "skillstab" ! A.class_ "tab" $ mconcat
        [ "skills"
        ]
      ]
    , H.script ! A.src "/coal/snapshot/main.js" $ ""
    ]
  handling (Get "/coal/snapshot/main.js") . pure . FS (FSReadConfig "assets/coal/snapshot") $ FSRead "js/main.js"
  handling (Get "/coal/snapshot/:playerid") $ FS (FSReadConfig snapshotStore) . FSRead <$> param "playerid" 
  handling (Put "/coal/snapshot/:playerid") $ do
    playerid <- param "playerid"
    snapshot <- bodyJSON
    time <- liftIO getCurrentTime
    setSnapshot playerid $ snapshot { snapshotTime = Just time }
    pure $ Plaintext "Snapshot update successful"
  handling (Get "/coal/snapshot/info/:playerid") $ JSON <$> (param "playerid" >>= getInfo)

stylesheet :: C.Css
stylesheet = mconcat
  [ "*" ? mconcat
    [ C.margin (C.S.px 0) (C.S.px 0) (C.S.px 0) (C.S.px 0)
    , C.fontFamily [] [C.monospace]
    ]
  , "html" ? C.height (C.S.pct 100)
  , "body" ? mconcat
    [ C.height (C.S.pct 100)
    , C.backgroundColor C.white
    ]
  , "#topbar" ? mconcat
    [ C.width (C.S.pct 100)
    , C.height (C.S.px 32)
    , C.position C.absolute
    , C.backgroundColor C.grey
    ]
  , "#upload" ? C.float C.floatLeft
  , "#profile" ? C.float C.floatLeft <> C.marginLeft (C.S.px 0)
  , "#iotm" ? C.float C.floatLeft <> C.marginLeft (C.S.px 0)
  , "#skills" ? C.float C.floatLeft <> C.marginLeft (C.S.px 0)
  , "#snapshot" ? C.float C.floatRight 
  , "#playerid" ? C.float C.floatRight <> C.marginRight (C.S.px 0)
  , "#uploadtext" ? mconcat
    [ C.width (C.S.pct 100)
    , C.height (C.S.pct 75)
    , C.boxSizing C.borderBox
    , C.marginTop (C.S.px 0)
    , C.marginBottom (C.S.px 0)
    , C.marginLeft (C.S.px 0)
    , C.marginRight (C.S.px 0)
    ]
  , "#uploadplayerid" ? C.marginLeft (C.S.px 0)
  , "#profilename" ? mconcat
    [ C.size (C.S.pct 200)
    , C.fontWeight C.bold
    ]
  , ".tab" ? mconcat
    [ C.position C.absolute
    , C.boxSizing C.borderBox
    , C.top (C.S.px 32)
    , C.bottom (C.S.px 8)
    , C.display C.none
    , C.width (C.S.pct 100)
    , C.paddingTop (C.S.px 8)
    , C.paddingLeft (C.S.px 8)
    , C.paddingRight (C.S.px 8)
    ]
  , ".hidden" ? C.opacity 0 <> C.visibility C.hidden
  , ".greenbox" ? mconcat
    [ C.backgroundColor C.lightgreen
    ]
  , ".whitebox" ? mconcat
    [ C.backgroundColor C.lightgrey
    ]
  , C.a ? mconcat
    [ C.fontWeight C.bold
    , C.color C.black
    ]
  , C.button <> C.input <> C.textarea ? mconcat
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

getSnapshot :: (MonadIO m, MonadCatch m) => Text -> m Snapshot
getSnapshot playerid = inputting (FSReadConfig snapshotStore) $ getJSON (FSRead playerid)

setSnapshot :: (MonadIO m, MonadCatch m) => Text -> Snapshot -> m ()
setSnapshot playerid board = outputting (FSWriteConfig snapshotStore) $ putJSON (FSWrite playerid) board

getInfo :: (MonadIO m, MonadCatch m) => Text -> Text -> Text -> m Info
getInfo botuser botpass playerid = do
  info <- catch
    (inputting (FSReadConfig infoStore) $ getJSON (FSRead playerid))
    (\(_ :: SomeException) -> getNew)
  time <- liftIO getCurrentTime
  if diffUTCTime time (infoTime info) < nominalDay
    then pure info
    else getNew
  where getNew = do
          new <- scrapeInfo botuser botpass playerid
          outputting (FSWriteConfig infoStore) $ putJSON (FSWrite playerid) new
          pure new
