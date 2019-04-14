module Saturnal.State where

import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Array (any, filter, head, index, length, range, zip, (:))
import Data.Eq ((==))
import Data.Field ((/))
import Data.Foldable (foldr)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.HeytingAlgebra ((&&))
import Data.Int (rem, toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (clamp, (<), (>=))
import Data.Ring ((-))
import Data.Semiring ((*), (+))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Math (sqrt)
import Saturnal.Types (Board(..), Card(..), Cell(..), Entity(..), Move, Player(..))

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

adjacentCells :: Board -> Tuple Int Int -> Array (Tuple Int Int)
adjacentCells (Board b) (Tuple x y) = filter (\(Tuple x' y') -> x' >= 0 && x' < b.boardWidth && y' >= 0 && y' < b.boardHeight) points
  where points :: Array (Tuple Int Int)
        points = if rem y 2 == 0
                 then [Tuple (x - 1) (y - 1), Tuple x (y - 2), Tuple x (y - 1), Tuple (x - 1) (y + 1), Tuple x (y + 2), Tuple x (y + 1)]
                 else [Tuple x (y - 1), Tuple x (y - 2), Tuple (x + 1) (y - 1), Tuple x (y + 1), Tuple x (y + 2), Tuple (x + 1) (y + 1)]

locateEntity :: Board -> String -> Maybe (Tuple Int Int)
locateEntity (Board b) uuid = head $ foldr (\x acc -> case x of Just p -> p:acc
                                                                Nothing -> acc) [] flipped
  where numbered :: Array (Tuple (Array Cell) Int)
        numbered = zip b.boardCells (range 0 (length b.boardCells))
        cellhas :: Cell -> Boolean
        cellhas (Cell c) = any (\(Entity e) -> e.entityID == uuid) c.cellEntities
        rowhas :: Array (Tuple Cell Int) -> Maybe Int
        rowhas = map snd <<< head <<< filter (cellhas <<< fst)
        selected :: Array (Tuple (Maybe Int) Int)
        selected = (\(Tuple row i) -> Tuple (rowhas <<< zip row <<< range 0 $ length row) i) <$> numbered
        flipped :: Array (Maybe (Tuple Int Int))
        flipped = ((\p -> case p of Tuple (Just x) y -> Just (Tuple x y)
                                    _ -> Nothing) <$> selected)

selectEntity :: Board -> String -> Maybe Entity
selectEntity b uuid =
  case locateEntity b uuid of
    Nothing -> Nothing
    Just pos ->
      case cellAt b pos of
        Nothing -> Nothing
        Just (Cell c) -> head $ filter matchingID c.cellEntities
  where matchingID :: Entity -> Boolean
        matchingID (Entity e) = e.entityID == uuid

selectPlayer :: Board -> String -> Maybe Player
selectPlayer (Board b) player = head $ filter (\(Player p) -> p.playerName == player) b.boardPlayers

selectCardHand :: Board -> String -> String -> Maybe Card
selectCardHand (Board b) uuid player = do
  (Player p) <- selectPlayer (Board b) player
  head $ filter (\(Card c) -> c.cardID == uuid) p.playerHand
