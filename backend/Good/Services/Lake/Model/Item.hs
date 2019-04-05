module Good.Services.Lake.Model.Item where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Services.Lake.Interface.Glyph
import Good.Services.Lake.Model.Material
import Good.Services.Lake.Model.Component

data Item = Item { itemId :: Text
                 , itemComponents :: [Component]
                 , itemName :: Text
                 , itemGlyph :: Glyph
                 , itemEssentialMaterial :: Maybe Material
                 }
  deriving (Show, Generic)
instance FromJSON Item where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Item where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }
