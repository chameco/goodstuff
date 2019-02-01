module Good.Services.PureSolarEmbrace.Types where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), Options, genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

opts :: Options
opts = defaultOptions { allNullaryToStringTag = False }

data TileSpec = TileSpec { tileSpecID :: Text
                         , tileSpecFloor :: Maybe Text
                         , tileSpecCeiling :: Maybe Text
                         , tileSpecFront :: Maybe Text
                         , tileSpecLeft :: Maybe Text
                         , tileSpecRight :: Maybe Text
                         }
              deriving (Show, Generic)
instance FromJSON TileSpec where
  parseJSON = genericParseJSON opts
instance ToJSON TileSpec where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Tile = TileUnique { tileUniqueSpec :: TileSpec }
          | Tile { tileSpec :: Text }
          deriving (Show, Generic)
instance FromJSON Tile where
  parseJSON = genericParseJSON opts
instance ToJSON Tile where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Zone = Zone { zoneID :: Text
                 , zoneTileSpecs :: [TileSpec]
                 , zoneMap :: [[Tile]]
                 }
          deriving (Show, Generic)
instance FromJSON Zone where
  parseJSON = genericParseJSON opts
instance ToJSON Zone where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Kenning = Kenning
             deriving (Show, Generic)
instance FromJSON Kenning where
  parseJSON = genericParseJSON opts
instance ToJSON Kenning where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data PlayerPublic = PlayerPublic { playerID :: Text
                                 , playerName :: Text
                                 , playerLevel :: Integer
                                 , playerKenning :: Kenning
                                 }
                  deriving (Show, Generic)

data PlayerPrivate = PlayerPrivate { playerHealth :: Integer
                                   , playerWealth :: Integer
                                   , playerFrequency :: Integer
                                   }
                   deriving (Show, Generic)

data PlayerHidden = PlayerHidden {
                                 }
                  deriving Show
