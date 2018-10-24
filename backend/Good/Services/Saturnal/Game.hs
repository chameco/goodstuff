module Good.Services.Saturnal.Game where

import Good.Prelude

import Good.Services.Saturnal.Types

processTurn :: Turn -> Board -> Board
processTurn t b = foldr (processMove (turnPlayer t)) b $ turnMoves t

processMove :: Text -> Move -> Board -> Board
processMove _ _ b = b

updateCell :: Board -> (Cell -> Cell) -> (Int, Int) -> Board
updateCell b f (x, y)
  | x < 0 || x >= boardWidth b || y < 0 || y >= boardHeight b = b
  | otherwise =
      let beforerows = take x $ boardCells b
          rows = drop y $ boardCells b
          row = headMay rows
          afterrows = drop 1 rows
      in case row of
           Nothing -> b
           Just r ->
             let beforecells = take y r
                 cells = drop y r
                 cell = headMay cells
                 aftercells = drop 1 cells
             in case cell of
                  Nothing -> b
                  Just c -> b { boardCells = beforerows <> [beforecells <> [f c] <> aftercells] <> afterrows }

spawnEntity :: Board -> Entity -> (Int, Int) -> Board
spawnEntity b e = updateCell b (\c -> c { cellEntities = e:cellEntities c })
