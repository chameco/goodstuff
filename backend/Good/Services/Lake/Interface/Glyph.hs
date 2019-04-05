module Good.Services.Lake.Interface.Glyph where

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Prelude

data Color = Color { colorForeground :: ()
                   , colorBackground :: ()
                   }
  deriving (Show, Generic)
instance FromJSON Color where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Color where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

data Frame = Frame { frameChar :: Char
                   , frameBackupChar :: Maybe Char
                   , frameColor :: Color 
                   }
  deriving (Show, Generic)
instance FromJSON Frame where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Frame where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

newtype Glyph = Glyph { glyphFrames :: [Frame]
                      }
  deriving (Show, Generic)
instance FromJSON Glyph where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Glyph where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }
