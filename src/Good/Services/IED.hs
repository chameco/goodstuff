module Good.Services.IED where

import Good.Prelude

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?))
import qualified Clay as C
import qualified Clay.Size as C.S
import qualified Clay.Render as C.R
import qualified Clay.Border as C.B
import qualified Clay.Font as C.F

import Good.Interfaces.Web

api :: Serving IO ()
api = do
  handling (Get "/ied/state") . pure . Markup . H.docTypeHtml $ mconcat
    [ H.head $ mconcat
      [ H.title "Folio: A Personalized Hydroponics System"
      , H.style . H.toHtml $ C.renderWith C.R.compact [] stylesheet
      ]
    , H.body $ mconcat
      [ H.h1 "Folio: A Personalized Hydroponics System"
      , H.table $ mconcat
        [ H.tr $ mconcat [ H.td $ H.span ! A.class_ "column-header" $ "Key" , H.td $ H.span ! A.class_ "column-header" $ "Value"]
        , H.tr $ mconcat [ H.td "Light" , H.td "On" ]
        ]
      ]
    ]
  handling (Post "/ied/state") $ do
    var <- param "var"
    val <- param "val"
    pure (Plaintext $ mconcat ["set ", var, ":", val])

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
