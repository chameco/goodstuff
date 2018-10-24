module Saturnal.State where

import Control.Bind (bind)
import Data.Array (index)
import Data.Field ((/))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (clamp)
import Data.Ring ((*))
import Data.Tuple (Tuple)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Math (sqrt)
import Saturnal.Types (Board(..), Cell, Move)

type Viewport = { r :: Number, x :: Number, y :: Number, selected :: Maybe (Tuple Int Int)}
data State = State Viewport (Array Move) Board

foreign import _setState :: Unit -> State -> Effect Unit

setState :: State -> Effect Unit
setState = _setState unit

foreign import _getState :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe State)

getState :: Effect (Maybe State)
getState = _getState Just Nothing

boundCamera :: State -> State
boundCamera (State v m board) = State (v { x = clamp 0.0 (3.0 * v.r * case board of Board b -> toNumber b.boardWidth) v.x
                                         , y = clamp 0.0 ((sqrt 3.0 / 2.0) * v.r * case board of Board b -> toNumber b.boardHeight) v.y
                                         }) m board

cellAt :: Board -> Int -> Int -> Maybe Cell
cellAt (Board b) x y = do
  row <- index b.boardCells y
  index row x
