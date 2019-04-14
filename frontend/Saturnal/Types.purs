module Saturnal.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)

opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

data TagData = TagDataInt Int
             | TagDataPoint Int Int
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

data EventHandler = EventHandler String String
derive instance genericEventHandler :: Generic EventHandler _
instance showEventHandler :: Show EventHandler where show = genericShow
instance decodeEventHandler :: Decode EventHandler where decode = genericDecode opts
instance encodeEventHandler :: Encode EventHandler where encode = genericEncode opts

data ActionDescription = ActionDescription { actionName :: String
                                           , actionDisplay :: String
                                           }
derive instance genericActionDescription :: Generic ActionDescription _
instance showActionDescription :: Show ActionDescription where show = genericShow
instance decodeActionDescription :: Decode ActionDescription where decode = genericDecode opts
instance encodeActionDescription :: Encode ActionDescription where encode = genericEncode opts

data Template = TemplateEntity { templateEntityName :: String
                               , templateEntityRank :: Int
                               , templateEntityActions :: Array ActionDescription
                               , templateEntityTags :: Array Tag
                               , templateEntityEventHandlers :: Array EventHandler
                               , templateEntityTemplates :: Array Template
                               }
              | TemplateStructure { templateStructureName :: String
                                  , templateStructureActions :: Array ActionDescription
                                  , templateStructureTag :: Array Tag
                                  , templateStructureEventHandlers :: Array EventHandler
                                  , templateStructureTemplates :: Array Template
                                  }
              | TemplateResource { templateResourceName :: String
                                 , templateResourceEventHandlers :: Array EventHandler
                                 , templateResourceTemplates :: Array Template
                                 }
              | TemplateCard { templateCardName :: String
                             , templateCardCost :: Int
                             , templateCardEffect :: String
                             , templateCardTemplates :: Array Template
                             }
              | TemplateSidecard { templateSidecardName :: String
                                 , templateSidecardEffect :: String
                                 , templateSidecardTemplates :: Array Template
                                 }
derive instance genericTemplate :: Generic Template _
instance showTemplate :: Show Template where show x = genericShow x
instance decodeTemplate :: Decode Template where decode x = genericDecode opts x
instance encodeTemplate :: Encode Template where encode x = genericEncode opts x

data Entity = Entity { entityID :: String
                     , entityName :: String
                     , entityOwner :: String
                     , entityRank :: Int
                     , entityActions :: Array ActionDescription
                     , entityTags :: Array Tag
                     , entityEventHandlers :: Array EventHandler
                     , entityTemplates :: Array Template
                     }
derive instance genericEntity :: Generic Entity _
instance showEntity :: Show Entity where show = genericShow
instance decodeEntity :: Decode Entity where decode = genericDecode opts
instance encodeEntity :: Encode Entity where encode = genericEncode opts

data Structure = Structure { structureID :: String
                           , structureName :: String
                           , structureOwner :: String
                           , structureActions :: Array ActionDescription
                           , structureTags :: Array Tag
                           , structureEventHandlers :: Array EventHandler
                           , structureTemplates :: Array Template
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
                 , cellEntities :: Array Entity
                 , cellStructures :: Array Structure
                 }
derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where show = genericShow
instance decodeCell :: Decode Cell where decode = genericDecode opts
instance encodeCell :: Encode Cell where encode = genericEncode opts

data Resource = Resource { resourceID :: String
                         , resourceName :: String
                         , resourceQuantity :: Int
                         , resourceEventHandlers :: Array EventHandler
                         , resourceTemplates :: Array Template
                         }
derive instance genericResource :: Generic Resource _
instance showResource :: Show Resource where show = genericShow
instance decodeResource :: Decode Resource where decode = genericDecode opts
instance encodeResource :: Encode Resource where encode = genericEncode opts

data Card = Card { cardID :: String
                 , cardName :: String
                 , cardCost :: Int
                 , cardEffect :: String
                 , cardTemplates :: Array Template
                 }
derive instance genericCard :: Generic Card _
instance showCard :: Show Card where show = genericShow
instance decodeCard :: Decode Card where decode = genericDecode opts
instance encodeCard :: Encode Card where encode = genericEncode opts

data Sidecard = Sidecard { sidecardID :: String
                         , sidecardName :: String
                         , sidecardEffect :: String
                         , sidecardTemplates :: Array Template
                         }
derive instance genericSidecard :: Generic Sidecard _
instance showSidecard :: Show Sidecard where show = genericShow
instance decodeSidecard :: Decode Sidecard where decode = genericDecode opts
instance encodeSidecard :: Encode Sidecard where encode = genericEncode opts

data Deck = Deck { deckMain :: Array Card
                 , deckSide :: Array Sidecard
                 }
derive instance genericDeck :: Generic Deck _
instance showDeck :: Show Deck where show = genericShow
instance decodeDeck :: Decode Deck where decode = genericDecode opts
instance encodeDeck :: Encode Deck where encode = genericEncode opts

data Player = Player { playerName :: String
                     , playerColor :: String
                     , playerHand :: Array Card
                     , playerDeck :: Deck
                     , playerResourceAlpha :: Resource
                     , playerResourceBeta :: Resource
                     , playerResourceGamma :: Resource
                     , playerResourceDelta :: Resource
                     , playerResources :: Array Resource
                     }
derive instance genericPlayer :: Generic Player _
instance showPlayer :: Show Player where show = genericShow
instance decodePlayer :: Decode Player where decode = genericDecode opts
instance encodePlayer :: Encode Player where encode = genericEncode opts

data Board = Board { boardCells :: Array (Array Cell)
                   , boardWidth :: Int
                   , boardHeight :: Int
                   , boardTurn :: Int
                   , boardPlayers :: Array Player
                   }
derive instance genericBoard :: Generic Board _
instance showBoard :: Show Board where show = genericShow
instance decodeBoard :: Decode Board where decode = genericDecode opts
instance encodeBoard :: Encode Board where encode = genericEncode opts

data Move = MoveEntity { moveEntityID :: String
                       , moveEntityAction :: String
                       , moveEntityX :: Int
                       , moveEntityY :: Int
                       }
          | MoveDraw
          | MoveCard { moveCardID :: String
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

data Faction = Faction { factionID :: String
                       , factionName :: String
                       , factionTemplates :: Array Template
                       }
derive instance genericFaction :: Generic Faction _
instance showFaction :: Show Faction where show = genericShow
instance decodeFaction :: Decode Faction where decode = genericDecode opts
instance encodeFaction :: Encode Faction where encode = genericEncode opts
