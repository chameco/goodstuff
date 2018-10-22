module Saturnal.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)

opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

data TagData = TagDataInt { tagDataInt :: Int }
             | TagDataPoint { tagDataPointX :: Int
                            , tagDataPointY :: Int
                            }
derive instance genericTagData :: Generic TagData _
instance showTagData :: Show TagData where show = genericShow
instance decodeTagData :: Decode TagData where decode = genericDecode opts
instance encodeTagData :: Encode TagData where encode = genericEncode opts

data Tag = Tag String
         | TagData String TagData
derive instance genericTag :: Generic Tag _
instance showTag :: Show Tag where show = genericShow
instance decodeTag :: Decode Tag where decode = genericDecode opts
instance encodeTag :: Encode Tag where encode = genericEncode opts

data Unit = Unit { unitOwner :: String
                 , unitRank :: Int
                 , unitTags :: Array Tag
                 }
derive instance genericUnit :: Generic Unit _
instance showUnit :: Show Unit where show = genericShow
instance decodeUnit :: Decode Unit where decode = genericDecode opts
instance encodeUnit :: Encode Unit where encode = genericEncode opts

data Structure = Structure { structureOwner :: String
                           , structureRank :: Int
                           , structureTags :: Array Tag
                           }
derive instance genericStructure :: Generic Structure _
instance showStructure :: Show Structure where show = genericShow
instance decodeStructure :: Decode Structure where decode = genericDecode opts
instance encodeStructure :: Encode Structure where encode = genericEncode opts

data CellType = CellBlack | CellGrey | CellWhite
derive instance genericCellType :: Generic CellType _
instance showCellType :: Show CellType where show = genericShow
instance decodeCellType :: Decode CellType where decode = genericDecode opts
instance encodeCellType :: Encode CellType where encode = genericEncode opts

data Cell = Cell { cellType :: CellType
                 , cellTags :: Array Tag
                 , cellUnits :: Array Unit
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
