module Good.Services.Saturnal where

import Good.Prelude

import qualified Text.Blaze.Html5 as H

import Good.Interfaces.Web

api :: Serving IO ()
api = handling (Get "/saturnal") $ do
  client <- readFile "assets/js/saturnal.js"
  pure . Markup . H.docTypeHtml $ mconcat
    [ H.head $ mconcat
      [ H.title "Saturnal"
      , H.script $ H.toHtml (toSL client :: Text)
      ]
    , H.body ""
    ]
