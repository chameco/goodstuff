module Good.Services.Saturnal where

import Good.Prelude

import qualified Text.Blaze.Html5 as H

import Good.Architecture.Inputs.FSRead
import Good.Interfaces.Web

api :: Serving IO ()
api = do
  handling (Get "/saturnal") $ do
    client <- readFile "assets/js/saturnal.js"
    pure . Markup . H.docTypeHtml $ mconcat
      [ H.head $ mconcat
        [ H.title "Saturnal"
        , H.script $ H.toHtml (toSL client :: Text)
        ]
      , H.body ""
      ]
  handling (Get "/saturnal/client.js") . pure . FS (FSReadConfig "assets/saturnal") $ FSRead "js/saturnal.js"
  pure ()
