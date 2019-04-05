module Good.Services.Lake.Model.Chunk where

import Good.Prelude

import Data.Vector (Vector)
-- import qualified Data.Vector as Vector
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Services.Lake.Model.Tile
import Good.Services.Lake.Model.Entity
import Good.Services.Lake.Model.Item

data ChunkCell = ChunkCell { chunkCellWall :: Tile
                           , chunkCellFloor :: Tile
                           , chunkCellEntity :: Maybe Entity
                           , chunkCellItems :: [Item]
                           }
  deriving (Show, Generic)
instance FromJSON ChunkCell where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON ChunkCell where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

newtype Chunk = Chunk { chunkCells :: Vector ChunkCell
                      }
  deriving (Show, Generic)
instance FromJSON Chunk where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Chunk where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }
