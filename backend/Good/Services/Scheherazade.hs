module Good.Services.Scheherazade where

import Good.Prelude

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?))
import qualified Clay as C
import qualified Clay.Size as C.S
import qualified Clay.Render as C.R

import Good.Interfaces.Web

api :: Serving IO ()
api = handling (Get "/") . pure . Markup . H.docTypeHtml $ mconcat
  [ H.head $ mconcat
    [ H.title "Scheherazade"
    , H.style . H.toHtml $ C.renderWith C.R.compact [] stylesheet
    ]
  , H.body $ mconcat
    [ H.div ! A.id "name" $ mconcat
      [ H.div ! A.class_ "big" $ "Sche"
      , H.div ! A.class_ "big" $ "hera"
      , H.div ! A.class_ "big" $ "zade"
      ]
    , H.div ! A.id "info" $ mconcat
      [ H.p "Software."
      , H.p "Mathematics."
      , H.p "Security."
      , H.br
      , H.a "Negotiate."
      ]
    ]
  ]

stylesheet :: C.Css
stylesheet = mconcat
  [ "body" ? mconcat
    [ C.backgroundColor C.white
    , C.maxWidth (C.S.pct 80)
    , C.marginLeft (C.S.pct 10)
    , C.marginRight (C.S.pct 10)
    ]
  , "a" ? mconcat
    [ C.fontColor C.black
    , C.fontStyle C.normal
    , C.fontWeight C.bold
    ]
  , ".big" ? mconcat
    [ C.fontSize (C.S.pct 1000)
    , C.fontWeight C.bold
    ]
  , "#name" ? C.float C.floatLeft
  , "#info" ? mconcat
    [ C.float C.floatRight
    , C.fontStyle C.italic
    , C.marginRight (C.S.pct 20)
    , C.marginTop (C.S.pct 10)
    ]
  ]
