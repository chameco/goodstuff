module Good.Services.Sim.Item where

import Good.Prelude

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

data Item = Item
  { itemId :: Text
  , itemName :: Text
  , itemDescription :: Text
  } deriving (Show, Eq)

buildItem :: MonadIO m => Text -> Text -> m Item
buildItem name desc = liftIO (toText <$> nextRandom) >>=
  \uuid -> pure $ Item uuid name desc
