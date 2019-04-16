module Good.Services.Sim.Gateway where

import Good.Prelude

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

data Gateway = Gateway
  { gatewayId :: Text
  , gatewayName :: Text
  , gatewayDescription :: Text
  , gatewayTarget :: Text
  } deriving (Show, Eq)

buildGateway :: MonadIO m => Text -> Text -> Text -> m Gateway
buildGateway name desc target = liftIO (toSL . toText <$> nextRandom) >>=
  \uuid -> pure $ Gateway uuid name desc target
