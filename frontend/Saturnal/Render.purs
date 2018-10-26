module Saturnal.Render where

import Control.Bind (bind, discard, pure)
import Control.Semigroupoid ((<<<))
import Data.Array (drop, filter, head, length, range, reverse, sortWith, zip)
import Data.Eq ((==))
import Data.Field ((/))
import Data.Function (($))
import Data.Functor (void, (<$>))
import Data.HeytingAlgebra ((&&))
import Data.Int (ceil, floor, rem, toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord (abs, max, min, (<), (<=), (>=))
import Data.Ring (negate, (*), (-))
import Data.Semiring ((+))
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Graphics.Canvas (Context2D, beginPath, closePath, fill, fillRect, lineTo, moveTo, restore, save, setFillStyle, setLineWidth, setStrokeStyle, stroke)
import Math (atan2, cos, pi, pow, sin, sqrt)
import Saturnal.Net (getPlayer)
import Saturnal.State (Viewport, locateEntity)
import Saturnal.Types (Board(..), Cell(..), CellType(..), Entity(..), Move(..))

renderHex :: Context2D -> String -> Number -> Number -> Number -> Effect Unit
renderHex ctx c r x y = do
  save ctx
  setFillStyle ctx c
  beginPath ctx
  moveTo ctx (x + r) y
  lineTo ctx (x + (r / 2.0)) (y - (r * sqrt 3.0 / 2.0)) 
  lineTo ctx (x - (r / 2.0)) (y - (r * sqrt 3.0 / 2.0)) 
  lineTo ctx (x - r) y
  lineTo ctx (x - (r / 2.0)) (y + (r * sqrt 3.0 / 2.0)) 
  lineTo ctx (x + (r / 2.0)) (y + (r * sqrt 3.0 / 2.0)) 
  closePath ctx
  fill ctx
  stroke ctx
  restore ctx

outlineHex :: Context2D -> String -> Number -> Number -> Number -> Effect Unit
outlineHex ctx c r x y = do
  save ctx
  setStrokeStyle ctx c
  setLineWidth ctx 5.0
  beginPath ctx
  moveTo ctx (x + r) y
  lineTo ctx (x + (r / 2.0)) (y - (r * sqrt 3.0 / 2.0))
  lineTo ctx (x - (r / 2.0)) (y - (r * sqrt 3.0 / 2.0))
  lineTo ctx (x - r) y
  lineTo ctx (x - (r / 2.0)) (y + (r * sqrt 3.0 / 2.0))
  lineTo ctx (x + (r / 2.0)) (y + (r * sqrt 3.0 / 2.0))
  closePath ctx
  stroke ctx
  restore ctx

renderPoly :: Context2D -> Int -> String -> Number -> Number -> Number -> Effect Unit
renderPoly ctx sides c r x y = do
  save ctx
  setFillStyle ctx c
  beginPath ctx
  moveTo ctx (x + r) y
  for_ (range 1 sides) $ \i -> let angle = toNumber i * 2.0 * pi / toNumber sides
                               in lineTo ctx (x + r * cos angle) (y + r * sin angle)
  closePath ctx
  fill ctx
  stroke ctx
  restore ctx

renderEntity :: Context2D -> Entity -> Number -> Number -> Effect Unit
renderEntity ctx (Entity e) x y = do
  save ctx
  setFillStyle ctx "#ff0000"
  fillRect ctx { x: x, y: y, width: 1.0, height: 1.0 }
  restore ctx

cellColor :: Cell -> String
cellColor (Cell cell) = "#eeeeee"

renderCell :: Context2D -> Tuple Number Number -> Tuple Number Number -> Cell -> Number -> Number -> Number -> Effect Unit
renderCell ctx (Tuple basex basey) (Tuple width height) cell@(Cell c) r x y = case c.cellType of
  CellWhite -> if x + r >= basex && x - r <= width && y + r >= basey && y - r <= height
               then do
                 renderHex ctx (cellColor cell) r x y
                 let sorted = reverse $ sortWith (\(Entity e) -> e.entityRank) c.cellEntities
                     highest = head sorted
                 case highest of
                   Just (Entity e) -> do
                     player <- getPlayer
                     let color = case player of Just p -> if p == e.entityOwner then "#77dd77" else "#ff6961"
                                                Nothing -> "#5f9f9f"
                     renderPoly ctx e.entityRank color (r / 3.0) x y
                   _ -> pure unit
               else pure unit
  _ -> pure unit

renderRow :: Context2D -> Tuple Number Number -> Tuple Number Number -> Number -> Number -> Number -> Array Cell -> Effect Unit
renderRow ctx base@(Tuple basex _) dims@(Tuple width _) r offset y row = void
                                                                         <<< traverse process
                                                                         <<< zip (drop start row)
                                                                         <<< range start
                                                                         <<< min (length row)
                                                                         $ ceil (width / (3.0 * r))
  where process :: Tuple Cell Int -> Effect Unit
        process (Tuple cell i) = renderCell ctx base dims cell r (offset + (toNumber i * r * 3.0)) y
        start :: Int
        start = max 0 $ floor (basex / (3.0 * r))

renderBoard :: Context2D -> Tuple Number Number -> Tuple Number Number -> Number -> Board -> Effect Unit
renderBoard ctx base@(Tuple _ basey) dims@(Tuple _ height) r (Board board) = void
                                                                             <<< traverse process
                                                                             <<< zip (drop start board.boardCells)
                                                                             <<< range start
                                                                             <<< min (length board.boardCells)
                                                                             $ ceil (2.0 * height / (sqrt 3.0 * r))
  where process :: Tuple (Array Cell) Int -> Effect Unit
        process (Tuple row i) = renderRow ctx base dims r (r * 1.5 * toNumber (rem i 2)) (toNumber i * r * sqrt 3.0 / 2.0) row
        start :: Int
        start = max 0 $ floor (2.0 * basey / (sqrt 3.0 * r))

renderArrow :: Context2D -> Tuple Number Number -> Tuple Number Number -> Effect Unit
renderArrow ctx (Tuple fromx fromy) (Tuple tox toy) = do
  let angle = atan2 (toy - fromy) (tox - fromx)
  save ctx
  setStrokeStyle ctx "#0000ff"
  setLineWidth ctx 2.0
  beginPath ctx
  moveTo ctx fromx fromy
  lineTo ctx tox toy
  lineTo ctx (tox - 10.0 * cos (angle - pi / 6.0)) (toy - 10.0 * sin (angle - pi / 6.0))
  moveTo ctx tox toy
  lineTo ctx (tox - 10.0 * cos (angle + pi / 6.0)) (toy - 10.0 * sin (angle + pi / 6.0))
  closePath ctx
  stroke ctx
  restore ctx

renderMove :: Context2D -> Viewport -> Board -> Move -> Effect Unit
renderMove ctx v b (MoveEntity m) =
  case locateEntity b m.moveEntityID of
    Just fromhex -> renderArrow ctx (hexToAbsolute v fromhex) $ hexToAbsolute v (Tuple m.moveEntityX m.moveEntityY)
    _ -> pure unit

viewportToAbsolute :: Viewport -> Tuple Number Number -> Tuple Number Number -> Tuple Number Number
viewportToAbsolute v (Tuple width height) (Tuple x y) = Tuple (x - (width / 2.0) + v.x) (y - (height / 2.0) + v.y)

hexToAbsolute :: Viewport -> Tuple Int Int -> Tuple Number Number
hexToAbsolute v (Tuple col row) = Tuple ((v.r * 1.5 * toNumber (rem row 2)) + (v.r * 3.0 * toNumber col)) (v.r * (sqrt 3.0 / 2.0) * toNumber row)

absoluteToHex :: Viewport -> Tuple Number Number -> Maybe (Tuple Int Int)
absoluteToHex v (Tuple x y)
  | x >= -v.r && y >= -v.r = let col = floor $ abs ((x + v.r) / (v.r * 1.5))
                                 row = floor $ abs (((y + v.r) - (v.r * toNumber (rem col 2))) / (v.r * sqrt 3.0))
                             in Just $ Tuple (col / 2) (row * 2 + rem col 2)
  | true = Nothing

nearestHex :: Viewport -> Board -> Tuple Number Number -> Tuple Number Number -> Maybe (Tuple Int Int)
nearestHex v (Board b) dims relative =
  case hexGuess of
    Just (Tuple x y) ->
      let guesses :: Array (Tuple Int Int)
          guesses = filter (\(Tuple x' y') -> x' >= 0 && x' < b.boardWidth && y' >= 0 && y' < b.boardHeight) [Tuple x (y - 1), Tuple (x - 1) (y - 1), Tuple x (y - 2), Tuple x (y - 1), Tuple (x + 1) (y - 1), Tuple x y, Tuple x (y + 1), Tuple (x - 1) (y + 1), Tuple x (y + 2), Tuple x (y + 1), Tuple (x + 1) (y + 1)]
          augmented :: Array (Tuple (Tuple Int Int) (Tuple Number Number))
          augmented = (\guess -> Tuple guess (hexToAbsolute v guess)) <$> guesses
          distances :: Array (Tuple (Tuple Int Int) Number)
          distances = (\(Tuple guess (Tuple hx hy)) -> Tuple guess (sqrt (pow (hx - ax) 2.0 + pow (hy - ay) 2.0))) <$> augmented
          sorted :: Array (Tuple (Tuple Int Int) Number)
          sorted = sortWith (\(Tuple _ d) -> d) distances
      in case head sorted of
        Just (Tuple h _) -> Just h
        Nothing -> Nothing
    Nothing -> Nothing
  where Tuple ax ay = viewportToAbsolute v dims relative
        hexGuess = absoluteToHex v $ Tuple ax ay
