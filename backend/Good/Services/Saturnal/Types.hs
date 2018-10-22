module Good.Services.Saturnal.Types where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), Options, genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

opts :: Options
opts = defaultOptions { allNullaryToStringTag = False }

-- Tags are used for optional data attached to other objects
data TagData = TagDataInt { tagDataInt :: Int }
             | TagDataPoint { tagDataPointX :: Int
                            , tagDataPointY :: Int
                            }
             | TagDataUnit { tagDataUnit :: Unit }
             deriving (Show, Generic)
instance FromJSON TagData where
  parseJSON = genericParseJSON opts
instance ToJSON TagData where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

-- Make sure that the first field of any tag is human-readable
data Tag = Tag Text
         | TagData Text TagData
         deriving (Show, Generic)
instance FromJSON Tag where
  parseJSON = genericParseJSON opts
instance ToJSON Tag where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Unit = Unit { unitOwner :: Text
                 , unitRank :: Int
                 , unitTags :: [Tag]
                 }
          deriving (Show, Generic)
instance FromJSON Unit where
  parseJSON = genericParseJSON opts
instance ToJSON Unit where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

-- A structure is a rankless immobile unit that can optionally spawn other units
-- (specified using "spawns" tag)
data Structure = Structure { structureOwner :: Text
                           , structureTags :: [Tag]
                           }
               deriving (Show, Generic)
instance FromJSON Structure where
  parseJSON = genericParseJSON opts
instance ToJSON Structure where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

-- Three types of cell:
--  - black (void, "hole in map", do not render)
--  - grey (obscured by fog of war)
--  - anything else
-- Terrain, etc. should be described in the tags
data CellType = CellBlack | CellGrey | CellWhite
              deriving (Show, Generic)
instance FromJSON CellType where
  parseJSON = genericParseJSON opts
instance ToJSON CellType where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Cell = Cell { cellType :: CellType
                 , cellTags :: [Tag]
                 , cellUnits :: [Unit]
                 , cellStructure :: Structure
                 }
          deriving (Show, Generic)
instance FromJSON Cell where
  parseJSON = genericParseJSON opts
instance ToJSON Cell where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Board = Board { boardCells :: [[Cell]]
                   , boardWidth :: Int
                   , boardHeight :: Int
                   , boardTurn :: Int
                   , boardPlayers :: [Text]
                   }
           deriving (Show, Generic)
instance FromJSON Board where
  parseJSON = genericParseJSON opts
instance ToJSON Board where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Move = MoveUnit { moveStartX :: Int
                     , moveStartY :: Int
                     , moveEndX :: Int
                     , moveEndY :: Int
                     }
          deriving (Show, Generic)
instance FromJSON Move where
  parseJSON = genericParseJSON opts
instance ToJSON Move where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Turn = Turn { turnPlayer :: Text
                 , turnMoves :: [Move]
                 , turnBid :: Int
                 }
          deriving (Show, Generic)
instance FromJSON Turn where
  parseJSON = genericParseJSON opts
instance ToJSON Turn where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts
