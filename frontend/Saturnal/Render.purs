module Saturnal.Render where

import Saturnal.Types

import Control.Alt (void)
import Data.Array (length, range, zip)
import Data.Int (rem, toNumber)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, beginPath, closePath, fill, lineTo, moveTo, setFillStyle, stroke)
import Math (sqrt)
import Prelude (Unit, discard, (<<<), ($), (+), (-), (*), (/))

renderHex :: Context2D -> String -> Number -> Number -> Number -> Effect Unit
renderHex ctx c r x y = do
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

cellColor :: Cell -> String
cellColor (Cell cell)  = case cell.cellType of
  CellBlack -> "#111111"
  CellGrey -> "#999999"
  CellWhite -> "#eeeeee"

renderCell :: Context2D -> Cell -> Number -> Number -> Number -> Effect Unit
renderCell ctx cell r x y = renderHex ctx (cellColor cell) r x y

renderRow :: Context2D -> Number -> Number -> Number -> Array Cell -> Effect Unit
renderRow ctx r offset y row = void <<< traverse process <<< zip row $ range 0 (length row)
  where process :: Tuple Cell Int -> Effect Unit
        process (Tuple cell i) = renderCell ctx cell r (offset + (toNumber i * r * 3.0)) y

renderBoard :: Context2D -> Number -> Board -> Effect Unit
renderBoard ctx r (Board board) = void <<< traverse process <<< zip board.boardCells $ range 0 (length board.boardCells)
  where process :: Tuple (Array Cell) Int -> Effect Unit
        process (Tuple row i) = renderRow ctx r (r * 1.5 * toNumber (rem i 2)) (toNumber i * r * sqrt 3.0 / 2.0) row
