module Good.Services.Coal.Snapshot.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), Options, genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Prelude

opts :: Options
opts = defaultOptions { allNullaryToStringTag = False }

data Snapshot = Snapshot
  { snapshotTime :: Maybe UTCTime
  , iotm :: [Bool]
  } deriving (Show, Generic)
instance FromJSON Snapshot where
  parseJSON = genericParseJSON opts
instance ToJSON Snapshot where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Skill = Skill
  { skillName :: Text
  , skillHCPerm :: Bool
  } deriving (Show, Generic)
instance FromJSON Skill where
  parseJSON = genericParseJSON opts
instance ToJSON Skill where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts

data Info = Info
  { infoTime :: UTCTime
  , name :: Text
  , title :: Text
  , avatar :: Text
  , skills :: [Skill]
  } deriving (Show, Generic)
instance FromJSON Info where
  parseJSON = genericParseJSON opts
instance ToJSON Info where
  toJSON = genericToJSON opts
  toEncoding = genericToEncoding opts
