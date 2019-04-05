module Good.Services.Lake.Model.Plane where

import Good.Prelude

import qualified Data.Map.Strict as Map
import Data.Aeson (FromJSON(..), ToJSON(..), object, pairs, withObject, (.=), (.:))

import Good.Architecture.Input
import Good.Architecture.Inputs.FSRead
import Good.Architecture.Output
import Good.Architecture.Outputs.FSWrite

import Good.Services.Lake.Utility.Error
import Good.Services.Lake.Utility.Coords
import Good.Services.Lake.Model.Chunk

planeStorePath :: Text
planeStorePath = "store/lake/plane"

planeStoreRead :: InputConfig FSRead
planeStoreRead = FSReadConfig planeStorePath

planeStoreWrite :: OutputConfig FSWrite
planeStoreWrite = FSWriteConfig planeStorePath

data Plane = Plane { planeName :: Maybe Text -- optional display name
                   , planeLoadedChunks :: Map Coords Chunk -- map chunk coordinates to chunks
                   }
  deriving Show
instance ToJSON Plane where
  toJSON p = object [ "planeName" .= planeName p
                    ]
  toEncoding p = pairs $ mconcat [ "planeName" .= planeName p
                                 ]
instance FromJSON Plane where
  parseJSON = withObject "Plane" $ \o -> do
    planeName <- o .: "planeName"
    let planeLoadedChunks = Map.empty
    pure Plane {..}

chunkPath :: ChunkIdentifier -> Text
chunkPath (pid, (x, y)) = mconcat [pid, "/", pack $ show x, "_", pack $ show y, ".json"]

planeEnsureChunkCached :: (MonadIO m, MonadCatch m) => Plane -> ChunkIdentifier -> m Plane
planeEnsureChunkCached p cid@(_, coords) =
  case lookup coords $ planeLoadedChunks p of
    Just _ -> pure p
    Nothing -> do
      chunk <- inputting planeStoreRead . getJSON . FSRead $ chunkPath cid
      pure $ p { planeLoadedChunks = Map.insert coords chunk $ planeLoadedChunks p }

saveChunk :: (MonadIO m, MonadCatch m) => ChunkIdentifier -> Chunk -> m ()
saveChunk cid chunk = outputting planeStoreWrite $ putJSON (FSWrite $ chunkPath cid) chunk

cleanupPlane :: (MonadIO m, MonadCatch m) => [ChunkIdentifier] -> Text -> Plane -> m (Maybe Plane)
cleanupPlane chunks pid p = do
  m <- foldM
    ( \cs (coords, chunk) ->
        if coords `elem` active
        then pure cs
        else saveChunk (pid, coords) chunk $> Map.delete coords cs
    )
    (planeLoadedChunks p)
    (Map.toList $ planeLoadedChunks p)
  if null m then pure Nothing else pure $ Just p { planeLoadedChunks = m }
  where inhabited :: [Coords]
        inhabited = snd <$> chunks
        active :: [Coords]
        active = concatMap adjacent inhabited

planeGetChunk :: (MonadIO m, MonadCatch m) => Plane -> ChunkIdentifier -> m (Plane, Chunk)
planeGetChunk p cid@(pid, coords) = do
  p' <- planeEnsureChunkCached p cid
  case lookup coords $ planeLoadedChunks p' of
    Just chunk -> pure (p', chunk)
    Nothing -> throwM . LakeError $ mconcat [ "Chunk in plane ", pid
                                            , " at ", pack $ show coords
                                            , " was improperly cached"]

planeUpdateChunk :: Plane -> ChunkIdentifier -> (Chunk -> Chunk) -> Plane
planeUpdateChunk p (_, coords) f = p { planeLoadedChunks = Map.adjust f coords $ planeLoadedChunks p }
