module Main where

import Prelude
import Saturnal.Render
import Saturnal.Types

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, logShow)
import Graphics.Canvas (CanvasElement, Context2D, clearRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, restore, save, setCanvasHeight, setCanvasWidth, translate)
import Signal (constant, foldp, runSignal, sampleOn)
import Signal.DOM (CoordinatePair, animationFrame, keyPressed, mouseButton, mousePos)
import Web.HTML (window)
import Web.HTML.Window (outerHeight, outerWidth)

data KeyState = KeyState Boolean Boolean Boolean Boolean Boolean Boolean
data InputState = InputState CoordinatePair Boolean KeyState
data GameState = GameState Number Number Number Board

screenToGame :: Tuple Number Number -> Tuple Number Number -> Tuple Number Number -> Tuple Number Number
screenToGame (Tuple cx cy) (Tuple w h) (Tuple x y) = Tuple (cx - w / 2.0 + x) (cy - h / 2.0 + y)

update :: InputState -> Effect GameState -> Effect GameState
update (InputState mouse pressed (KeyState left right up down minus plus)) gs = do
  game@(GameState r cx cy board) <- gs
  let leftx = if left then cx + 10.0 else cx
      x = if right then leftx - 10.0 else leftx
      upy = if up then cy + 10.0 else cy
      y = if down then upy - 10.0 else upy
      outr = if minus then r - 10.0 else r
      inr = if plus then outr + 10.0 else outr
  pure $ GameState inr x y board

render :: CanvasElement -> Context2D -> Effect GameState -> Effect Unit 
render canvas ctx gs = do
  (GameState r cx cy board) <- gs
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  clearRect ctx {x: 0.0, y: 0.0, width: width, height: height}
  save ctx
  translate ctx { translateX: cx - (width / 2.0), translateY: cy - (height / 2.0)}
  renderBoard ctx r board
  restore ctx

main :: Effect Unit
main = do
  getCanvasElementById "canvas" >>= \c -> case c of
    Nothing -> error "Canvas element not found"
    Just canvas -> do
      win <- window
      width <- outerWidth win
      height <- outerHeight win
      setCanvasWidth canvas $ toNumber width
      setCanvasHeight canvas $ toNumber height
      ctx <- getContext2D canvas
      let board = Board { boardCells: [ [Cell { cellType: CellWhite, cellTags: [] }, Cell { cellType: CellBlack, cellTags: [] }]
                                      , [Cell { cellType: CellWhite, cellTags: [] }, Cell { cellType: CellWhite, cellTags: [] }]
                                      , [Cell { cellType: CellWhite, cellTags: [] }, Cell { cellType: CellWhite, cellTags: [] }]
                                      , [Cell { cellType: CellWhite, cellTags: [] }, Cell { cellType: CellWhite, cellTags: [] }]
                                      ]
                        , boardWidth: 2
                        , boardHeight: 4
                        , boardTurn: 0
                        , boardPlayers : ["foo"]
                        }
      frames <- animationFrame
      mouse <- mousePos
      pressed <- mouseButton 1
      left <- keyPressed 37 
      right <- keyPressed 39 
      up <- keyPressed 38
      down <- keyPressed 40
      minus <- keyPressed 173
      plus <- keyPressed 61
      let keys = KeyState <$> left <*> right <*> up <*> down <*> minus <*> plus
      runSignal (render canvas ctx <$> foldp update (pure $ GameState 50.0 (toNumber width / 2.0) (toNumber height / 2.0) board) (sampleOn frames (InputState <$> mouse <*> pressed <*> keys)))
