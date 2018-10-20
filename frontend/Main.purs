module Main where

import Prelude
import Saturnal.Event
import Saturnal.Render
import Saturnal.Types

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error, log, logShow)
import Graphics.Canvas (CanvasElement, Context2D, clearRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, restore, save, setCanvasHeight, setCanvasWidth, translate)
import Web.HTML (window)
import Web.HTML.Window (outerHeight, outerWidth)

boundCamera :: State -> State
boundCamera (State v board) = State (v { x = clamp 0.0 (3.0 * v.r * case board of Board b -> toNumber b.boardWidth) v.x
                                       , y = clamp 0.0 (v.r * case board of Board b -> toNumber b.boardHeight) v.y
                                       }) board

render :: CanvasElement -> Context2D -> State -> Effect Unit 
render canvas ctx (State v board) = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  clearRect ctx {x: 0.0, y: 0.0, width: width, height: height}
  save ctx
  translate ctx { translateX: (width / 2.0) - v.x, translateY: (height / 2.0) - v.y}
  renderBoard ctx v.r board
  restore ctx

main :: Effect Unit
main = do
  getCanvasElementById "canvas" >>= \c -> case c of
    Nothing -> error "Canvas element not found"
    Just canvas -> do
      win <- window
      outerWidth win >>= toNumber >>> setCanvasWidth canvas
      outerHeight win >>= toNumber >>> setCanvasHeight canvas
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
      setState $ State { r: 50.0, x: 0.0, y: 0.0 } board
      key "ArrowLeft" <<< join <<< withState (pure unit) $ \(State v b) -> do
        setState <<< boundCamera $ State (v { x = v.x - 10.0 }) b
      key "ArrowRight" <<< join <<< withState (pure unit) $ \(State v b) -> do
        setState <<< boundCamera $ State (v { x = v.x + 10.0 }) b
      key "ArrowUp" <<< join <<< withState (pure unit) $ \(State v b) -> do
        setState <<< boundCamera $ State (v { y = v.y - 10.0 }) b
      key "ArrowDown" <<< join <<< withState (pure unit) $ \(State v b) -> do
        setState <<< boundCamera $ State (v { y = v.y + 10.0 }) b
      frames <<< join <<< withState (pure unit) $ render canvas ctx
