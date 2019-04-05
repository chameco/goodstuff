module Good.Services.Lake.Utility.Coords where

import Good.Prelude

chunkWidth :: Int
chunkWidth = 32

chunkHeight :: Int
chunkHeight = 32

type Coords = (Int, Int)

adjacent :: Coords -> [Coords]
adjacent (x, y) = [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
                  , (x - 1, y), (x + 1, y)
                  , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
                  ]

data Direction = North | South | West | East
               deriving (Show, Eq)

type ChunkIdentifier = (Text, Coords)

chunkAlong :: Direction -> ChunkIdentifier -> ChunkIdentifier
chunkAlong North (p, (x, y)) = (p, (x, y - 1))
chunkAlong South (p, (x, y)) = (p, (x, y + 1))
chunkAlong West (p, (x, y)) = (p, (x - 1, y))
chunkAlong East (p, (x, y)) = (p, (x + 1, y))

type GlobalCoords = (ChunkIdentifier, Coords)

chunkId :: GlobalCoords -> ChunkIdentifier
chunkId (cid, _) = cid

planeId :: GlobalCoords -> Text
planeId ((pid, _), _) = pid

chunkCoords :: GlobalCoords -> Coords
chunkCoords ((_, coords), _) = coords

localCoords :: GlobalCoords -> Coords
localCoords ((_, _), coords) = coords

along :: Direction -> GlobalCoords -> GlobalCoords
along North ((p, (cx, cy)), (x, 0)) = ((p, (cx, cy - 1)), (x, chunkHeight - 1))
along North ((p, (cx, cy)), (x, y)) = ((p, (cx, cy)), (x, y - 1))
along South ((p, (cx, cy)), (x, y))
  | y == chunkHeight - 1 = ((p, (cx, cy + 1)), (x, 0))
  | otherwise = ((p, (cx, cy)), (x, y + 1))
along West ((p, (cx, cy)), (0, y)) = ((p, (cx - 1, cy)), (chunkWidth - 1, y))
along West ((p, (cx, cy)), (x, y)) = ((p, (cx, cy)), (x - 1, y))
along East ((p, (cx, cy)), (x, y))
  | x == chunkWidth - 1 = ((p, (cx + 1, cy)), (0, y))
  | otherwise = ((p, (cx, cy)), (x + 1, y))
