module Good.Services.Sim.Room where

import Good.Prelude

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

data Room = Room
  { roomId :: Text
  , roomName :: Text
  , roomDescription :: Text
  , roomEntities :: [Text]
  , roomFeatures :: [Text]
  , roomGateways :: [Text]
  } deriving (Show, Eq)

buildRoom :: MonadIO m => Text -> Text -> [Text] -> [Text] -> [Text] -> m Room
buildRoom name desc es fs gs = liftIO (toSL . toText <$> nextRandom) >>=
  \uuid -> pure $ Room uuid name desc es fs gs
