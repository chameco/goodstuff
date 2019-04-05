module Good.Services.Lake.Model.Player where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Services.Lake.Model.Item

data PlayerStats = PlayerStats { playerStatsId :: Text -- administrative stats
                               -- meta-stats
                               , playerStatsMemory :: Int 
                               , playerStatsNobility :: Int
                               , playerStatsHonor :: Int
                               -- leveled stats
                               , playerStatsConstitution :: Int
                               , playerStatsEndurance :: Int
                               , playerStatsStrength :: Int
                               , playerStatsSkill :: Int
                               , playerStatsAccuracy :: Int
                               , playerStatsWisdom :: Int
                               -- secret stats
                               , playerStatsMythopoeticism :: Int
                               , playerStatsFilth :: Int
                               -- gameplay stats
                               , playerStatsInventory :: [Item]
                               , playerStatsHandLeft :: Item
                               , playerStatsHandRight :: Item
                               , playerStatsBodyShirt :: Item
                               , playerStatsBodySuit :: Item
                               , playerStatsBodyCloak :: Item
                               , playerStatsBodyHat :: Item
                               , playerStatsBodyGloveLeft :: Item
                               , playerStatsBodyGloveRight :: Item
                               , playerStatsBodyBootLeft :: Item
                               , playerStatsBodyBootRight :: Item
                               , playerStatsEntropy :: Int -- aggregate age, chronic illness, hunger, thirst
                               }
  deriving (Show, Generic)
instance FromJSON PlayerStats where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON PlayerStats where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }
