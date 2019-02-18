module Good.Services.Sim where

import Good.Prelude

import Data.Map (Map)
import qualified Data.Map as Map

data Item = Item
  { itemId :: Text
  , itemName :: Text
  , itemDescription :: Text
  }

data Entity = Entity
  { entityId :: Text
  , entityName :: Text
  , entityDescription :: Text
  , entityInventory :: [Text]
  }

data Feature = Feature
  { featureId :: Text
  , featureName :: Text
  , featureDescription :: Text
  }

data Gateway = Gateway
  { gatewayId :: Text
  , gatewayName :: Text
  , gatewayDescription :: Text
  , gatewayTarget :: Text
  }

data Room = Room
  { roomId :: Text
  , roomName :: Text
  , roomDescription :: Text
  , roomEntities :: [Text]
  , roomFeatures :: [Text]
  }

data Plane = Plane
  { planeId :: Text
  , planeName :: Text
  , planeDescription :: Text
  , planeRooms :: [Text]
  }

data Universe = Universe
  { items :: Map Text Item
  , entities :: Map Text Entity
  , features :: Map Text Feature
  , rooms :: Map Text Room
  , planes :: Map Text Plane
  }
