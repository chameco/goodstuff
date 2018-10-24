module Good.Services.Saturnal.Game where

import Good.Prelude

import Good.Services.Saturnal.Types

processTurn :: Turn -> Board -> Board
processTurn t b = foldr (processMove (turnPlayer t)) b $ turnMoves t

processMove :: Text -> Move -> Board -> Board
processMove player m@MoveEntity{} b = case entity of
  Just e -> spawnEntity (removeEntity b e (moveEntityStartX m, moveEntityStartY m)) e (moveEntityEndX m, moveEntityEndY m)
  Nothing -> b
  where startCell = cellAt b (moveEntityStartX m, moveEntityStartY m)
        entity = startCell >>= headMay . filter ((==player) . entityOwner) . filter ((==moveEntityID m) . entityID) . cellEntities

cellAt :: Board -> (Int, Int) -> Maybe Cell
cellAt b@Board{} (x, y) = do
  row <- index (boardCells b) y
  index row x

updateCell :: Board -> (Cell -> Cell) -> (Int, Int) -> Board
updateCell b f (x, y)
  | x < 0 || x >= boardWidth b || y < 0 || y >= boardHeight b = b
  | otherwise =
      let beforerows = take y $ boardCells b
          rows = drop y $ boardCells b
          row = headMay rows
          afterrows = drop 1 rows
      in case row of
           Nothing -> b
           Just r ->
             let beforecells = take x r
                 cells = drop x r
                 cell = headMay cells
                 aftercells = drop 1 cells
             in case cell of
                  Nothing -> b
                  Just c -> b { boardCells = beforerows <> [beforecells <> [f c] <> aftercells] <> afterrows }

removeEntity :: Board -> Entity -> (Int, Int) -> Board
removeEntity b e = updateCell b (\c -> c { cellEntities = filter ((/=entityID e) . entityID) $ cellEntities c })

spawnEntity :: Board -> Entity -> (Int, Int) -> Board
spawnEntity b e = updateCell b (\c -> c { cellEntities = e:cellEntities c })
