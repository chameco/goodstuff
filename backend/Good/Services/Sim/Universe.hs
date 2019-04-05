module Good.Services.Sim.Universe where

import Good.Prelude

import Data.Map.Strict (Map)

import Good.Services.Sim.Item
import Good.Services.Sim.Entity
import Good.Services.Sim.Feature
import Good.Services.Sim.Gateway
import Good.Services.Sim.Room
import Good.Services.Sim.Area

data Universe = Universe
  { items :: Map Text Item
  , entities :: Map Text Entity
  , features :: Map Text Feature
  , gateways :: Map Text Gateway
  , rooms :: Map Text Room
  , areas :: Map Text Area
  } deriving (Show, Eq)

instance Semigroup Universe where
  u1 <> u2 = Universe
    { items = items u1 <> items u2
    , entities = entities u1 <> entities u2
    , features = features u1 <> features u2
    , gateways = gateways u1 <> gateways u2
    , rooms = rooms u1 <> rooms u2
    , areas = areas u1 <> areas u2
    }

instance Monoid Universe where
  mempty = Universe
    { items = mempty
    , entities = mempty
    , features = mempty
    , gateways = mempty
    , rooms = mempty
    , areas = mempty
    }

lookupItem :: Universe -> Text -> Maybe Item
lookupItem u = flip lookup (items u)

lookupEntity :: Universe -> Text -> Maybe Entity
lookupEntity u = flip lookup (entities u)

lookupFeature :: Universe -> Text -> Maybe Feature
lookupFeature u = flip lookup (features u)

lookupGateway :: Universe -> Text -> Maybe Gateway
lookupGateway u = flip lookup (gateways u)

lookupRoom :: Universe -> Text -> Maybe Room
lookupRoom u = flip lookup (rooms u)

lookupArea :: Universe -> Text -> Maybe Area
lookupArea u = flip lookup (areas u)
