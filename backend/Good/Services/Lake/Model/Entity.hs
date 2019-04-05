module Good.Services.Lake.Model.Entity where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Services.Lake.Interface.Glyph
import Good.Services.Lake.Model.Material
import Good.Services.Lake.Model.Component
import Good.Services.Lake.Model.Player

data EntitySex = EntitySexMale
               | EntitySexFemale
               deriving (Show, Generic)
instance FromJSON EntitySex where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON EntitySex where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

data EntityStats = EntityStats { entityHP :: Int
                               , entitySpeed :: Int
                               , entityAttackModifier :: Int
                               , entityDefenseModifier :: Int
                               }
  deriving (Show, Generic)
instance FromJSON EntityStats where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON EntityStats where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

data Entity = Entity { entityComponents :: [Component]
                     , entityName :: Text
                     , entityGlyph :: Glyph
                     , entityEssentialMaterial :: Maybe Material
                     , entitySex :: Maybe EntitySex
                     , entityStats :: EntityStats
                     , entityPlayerStats :: Maybe PlayerStats
                     }
  deriving (Show, Generic)
instance FromJSON Entity where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Entity where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

isPlayer :: Entity -> Bool
isPlayer = isJust . entityPlayerStats
