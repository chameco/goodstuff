module Good.Services.Saturnal.Types where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), Options, genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

opts :: Options
opts = defaultOptions { allNullaryToStringTag = False }

-- Tags are used for optional data attached to other objects
data TagData = TagDataInt Int
             | TagDataPoint Int Int
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

data EventHandler = EventHandler Text Text
                  deriving (Show, Generic)
instance FromJSON EventHandler where
  parseJSON = genericParseJSON opts
instance ToJSON EventHandler where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data ActionDescription = ActionDescription { actionName :: Text
                                           , actionDisplay :: Text
                                           }
                       deriving (Show, Generic)
instance FromJSON ActionDescription where
  parseJSON = genericParseJSON opts
instance ToJSON ActionDescription where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Template = TemplateEntity { templateEntityRank :: Int
                               , templateEntityActions :: [ActionDescription]
                               , templateEntityTags :: [Tag]
                               , templateEntityEventHandlers :: [EventHandler]
                               , templateEntityTemplates :: [Template]
                               }
              | TemplateStructure { templateStructureActions :: [ActionDescription]
                                  , templateStructureTag :: [Tag]
                                  , templateStructureEventHandlers :: [EventHandler]
                                  , templateStructureTemplates :: [Template]
                                  }
              | TemplateResource { templateResourceName :: Text
                                 , templateResourceQuantity :: Int
                                 , templateResourceEventHandlers :: [EventHandler]
                                 , templateResourceTemplates :: [Template]
                                 }
              deriving (Show, Generic)
instance FromJSON Template where
  parseJSON = genericParseJSON opts
instance ToJSON Template where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Entity = Entity { entityID :: Text
                     , entityOwner :: Text
                     , entityRank :: Int
                     , entityActions :: [ActionDescription]
                     , entityTags :: [Tag]
                     , entityEventHandlers :: [EventHandler]
                     , entityTemplates :: [Template]
                     }
          deriving (Show, Generic)
instance FromJSON Entity where
  parseJSON = genericParseJSON opts
instance ToJSON Entity where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

-- A structure is a rankless immobile unit that can optionally spawn other units
-- (specified using "spawns" tag)
data Structure = Structure { structureID :: Text
                           , structureOwner :: Text
                           , structureActions :: [Text]
                           , structureTags :: [Tag]
                           , structureEventHandlers :: [EventHandler]
                           , structureTemplates :: [Template]
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
              deriving (Show, Eq, Generic)
instance FromJSON CellType where
  parseJSON = genericParseJSON opts
instance ToJSON CellType where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Cell = Cell { cellType :: CellType
                 , cellTags :: [Tag]
                 , cellEntities :: [Entity]
                 , cellStructures :: [Structure]
                 }
          deriving (Show, Generic)
instance FromJSON Cell where
  parseJSON = genericParseJSON opts
instance ToJSON Cell where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Resource = Resource { resourceName :: Text
                         , resourceQuantity :: Int
                         , resourceEventHandlers :: [EventHandler]
                         , resourceTemplates :: [Template]
                         }
              deriving (Show, Generic)
instance FromJSON Resource where
  parseJSON = genericParseJSON opts
instance ToJSON Resource where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Card = Card { cardName :: Text
                 , cardCost :: Int
                 , cardEffect :: Text
                 , cardTemplates :: [Template]
                 }
          deriving (Show, Generic)
instance FromJSON Card where
  parseJSON = genericParseJSON opts
instance ToJSON Card where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Sidecard = Sidecard { sidecardName :: Text
                         , sidecardEffect :: Text
                         , sidecardTemplates :: [Template]
                         }
              deriving (Show, Generic)
instance FromJSON Sidecard where
  parseJSON = genericParseJSON opts
instance ToJSON Sidecard where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Deck = Deck { deckMain :: [Card]
                 , deckSide :: [Sidecard]
                 }
          deriving (Show, Generic)
instance FromJSON Deck where
  parseJSON = genericParseJSON opts
instance ToJSON Deck where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Player = Player { playerName :: Text
                     , playerColor :: Text
                     , playerDeck :: Deck
                     , playerResourceAlpha :: Resource
                     , playerResourceBeta :: Resource
                     , playerResourceGamma :: Resource
                     , playerResourceDelta :: Resource
                     , playerResources :: [Resource]
                     }
              deriving (Show, Generic)
instance FromJSON Player where
  parseJSON = genericParseJSON opts
instance ToJSON Player where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Board = Board { boardCells :: [[Cell]]
                   , boardWidth :: Int
                   , boardHeight :: Int
                   , boardTurn :: Int
                   , boardPlayers :: [Player]
                   }
           deriving (Show, Generic)
instance FromJSON Board where
  parseJSON = genericParseJSON opts
instance ToJSON Board where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Move = MoveEntity { moveEntityID :: Text
                       , moveEntityAction :: Text
                       , moveEntityX :: Int
                       , moveEntityY :: Int
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
