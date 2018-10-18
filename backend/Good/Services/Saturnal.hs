module Good.Services.Saturnal where

import Good.Prelude

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?))
import qualified Clay as C
import qualified Clay.Size as C.S
import qualified Clay.Render as C.R

import Good.Architecture.Inputs.FSRead
import Good.Interfaces.Web

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
  pure ()

stylesheet :: C.Css
stylesheet = mconcat
  [ "#canvas" ? mconcat
    [ C.width $ C.S.pct 100
    , C.height $ C.S.pct 100
    ]
  ]
