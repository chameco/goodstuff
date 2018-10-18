module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Graphics.Canvas (fillPath, getCanvasElementById, getContext2D, rect, setFillStyle)

main :: Effect Unit
main = do
  getCanvasElementById "canvas" >>= \c -> case c of
    Nothing -> error "Canvas element not found"
    Just canvas -> do
      ctx <- getContext2D canvas
      setFillStyle ctx "#00ff00"
      fillPath ctx $ rect ctx { x: 0.0
                              , y: 0.0
                              , width: 100.0
                              , height: 100.0
                              }

