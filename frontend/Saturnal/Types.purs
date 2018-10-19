module Saturnal.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)

opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

data CellType = CellBlack | CellGrey | CellWhite
derive instance genericCellType :: Generic CellType _
instance showCellType :: Show CellType where show = genericShow
instance decodeCellType :: Decode CellType where decode = genericDecode opts
instance encodeCellType :: Encode CellType where encode = genericEncode opts

data CellTagData = CellTagDataInt { cellTagDataInt :: Int }
                 | CellTagDataPoint { cellTagDataPointX :: Int
                                    , cellTagDataPointY :: Int
                                    }
derive instance genericCellTagData :: Generic CellTagData _
instance showCellTagData :: Show CellTagData where show = genericShow
instance decodeCellTagData :: Decode CellTagData where decode = genericDecode opts
instance encodeCellTagData :: Encode CellTagData where encode = genericEncode opts

data CellTag = CellTag String
             | CellTagData String CellTagData
derive instance genericCellTag :: Generic CellTag _
instance showCellTag :: Show CellTag where show = genericShow
instance decodeCellTag :: Decode CellTag where decode = genericDecode opts
instance encodeCellTag :: Encode CellTag where encode = genericEncode opts

data Cell = Cell { cellType :: CellType
                 , cellTags :: Array CellTag
                 }
derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where show = genericShow
instance decodeCell :: Decode Cell where decode = genericDecode opts
instance encodeCell :: Encode Cell where encode = genericEncode opts

data Board = Board { boardCells :: Array (Array Cell)
                   , boardWidth :: Int
                   , boardHeight :: Int
                   , boardTurn :: Int
                   , boardPlayers :: Array String
                   }
derive instance genericBoard :: Generic Board _
instance showBoard :: Show Board where show = genericShow
instance decodeBoard :: Decode Board where decode = genericDecode opts
instance encodeBoard :: Encode Board where encode = genericEncode opts

data Move = MoveUnit { moveStartX :: Int
                     , moveStartY :: Int
                     , moveEndX :: Int
                     , moveEndY :: Int
                     }
derive instance genericMove :: Generic Move _
instance showMove :: Show Move where show = genericShow
instance decodeMove :: Decode Move where decode = genericDecode opts
instance encodeMove :: Encode Move where encode = genericEncode opts

data Turn = Turn { turnPlayer :: String
                 , turnMoves :: Array Move
                 , turnBid :: Int
                 }
derive instance genericTurn :: Generic Turn _
instance showTurn :: Show Turn where show = genericShow
instance decodeTurn :: Decode Turn where decode = genericDecode opts
instance encodeTurn :: Encode Turn where encode = genericEncode opts
