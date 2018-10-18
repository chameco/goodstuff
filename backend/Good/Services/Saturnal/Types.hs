module Good.Services.Saturnal.Types where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), Options, genericToEncoding, defaultOptions, allNullaryToStringTag)

opts :: Options
opts = defaultOptions { allNullaryToStringTag = False }

data CellType = CellBlack | CellGrey | CellWhite
              deriving (Show, Generic)
instance FromJSON CellType where
instance ToJSON CellType where
  toEncoding = genericToEncoding opts

data CellTagData = CellTagDataInt { cellTagDataInt :: Integer }
                 | CellTagDataPoint { cellTagDataPointX :: Integer
                                    , cellTagDataPointY :: Integer
                                    }
                 deriving (Show, Generic)
instance FromJSON CellTagData where
instance ToJSON CellTagData where toEncoding = genericToEncoding opts

data CellTag = CellTag Text
             | CellTagData Text CellTagData
             deriving (Show, Generic)
instance FromJSON CellTag where
instance ToJSON CellTag where toEncoding = genericToEncoding opts

data Cell = Cell { cellType :: CellType
                 , cellTags :: [CellTag]
                 }
          deriving (Show, Generic)
instance FromJSON Cell where
instance ToJSON Cell where toEncoding = genericToEncoding opts

data Board = Board { boardCells :: [[Cell]]
                   , boardWidth :: Int
                   , boardHeight :: Int
                   }
           deriving (Show, Generic)
instance FromJSON Board where
instance ToJSON Board where toEncoding = genericToEncoding opts
