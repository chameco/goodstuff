module Saturnal.State where

import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Array (any, filter, head, index, length, range, zip, (:))
import Data.Eq ((==))
import Data.Field ((/))
import Data.Foldable (foldr)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (clamp)
import Data.Ring ((*))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Math (sqrt)
import Saturnal.Types (Board(..), Cell(..), Entity(..), Move)

type Viewport = { r :: Number, x :: Number, y :: Number, primary :: Maybe (Tuple Int Int), secondary :: Maybe (Tuple Int Int)}
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

cellAt :: Board -> Tuple Int Int -> Maybe Cell
cellAt (Board b) (Tuple x y) = do
  row <- index b.boardCells y
  index row x

locateEntity :: Board -> Entity -> Maybe (Tuple Int Int)
locateEntity (Board b) (Entity e) = head $ foldr (\x acc -> case x of Just p -> p:acc
                                                                      Nothing -> acc) [] flipped
  where numbered :: Array (Tuple (Array Cell) Int)
        numbered = zip b.boardCells (range 0 (length b.boardCells))
        cellhas :: Cell -> Boolean
        cellhas (Cell c) = any (\(Entity e') -> e'.entityID == e.entityID) c.cellEntities
        rowhas :: Array (Tuple Cell Int) -> Maybe Int
        rowhas = map snd <<< head <<< filter (cellhas <<< fst)
        selected :: Array (Tuple (Maybe Int) Int)
        selected = (\(Tuple row i) -> Tuple (rowhas <<< zip row <<< range 0 $ length row) i) <$> numbered
        flipped :: Array (Maybe (Tuple Int Int))
        flipped = ((\p -> case p of Tuple (Just x) y -> Just (Tuple x y)
                                    _ -> Nothing) <$> selected)
