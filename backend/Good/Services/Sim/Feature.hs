module Good.Services.Sim.Feature where

import Good.Prelude

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

data Feature = Feature
  { featureId :: Text
  , featureName :: Text
  , featureDescription :: Text
  } deriving (Show, Eq)

buildFeature :: MonadIO m => Text -> Text -> m Feature
buildFeature name desc = liftIO (toText <$> nextRandom) >>=
  \uuid -> pure $ Feature uuid name desc
