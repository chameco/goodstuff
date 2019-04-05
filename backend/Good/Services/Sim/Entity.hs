module Good.Services.Sim.Entity where

import Good.Prelude

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

data Entity = Entity
  { entityId :: Text
  , entityName :: Text
  , entityDescription :: Text
  , entityInventory :: [Text]
  } deriving (Show, Eq)

buildEntity :: MonadIO m => Text -> Text -> [Text] -> m Entity
buildEntity name desc inv = liftIO (toText <$> nextRandom) >>=
  \uuid -> pure $ Entity uuid name desc inv
