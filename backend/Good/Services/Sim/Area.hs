module Good.Services.Sim.Area where

import Good.Prelude

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

data Area = Area
  { areaId :: Text
  , areaName :: Text
  , areaDescription :: Text
  , areaRooms :: [Text]
  } deriving (Show, Eq)

buildArea :: MonadIO m => Text -> Text -> [Text] -> m Area
buildArea name desc rooms = liftIO (toSL . toText <$> nextRandom) >>=
  \uuid -> pure $ Area uuid name desc rooms
