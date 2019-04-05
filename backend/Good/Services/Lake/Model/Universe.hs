module Good.Services.Lake.Model.Universe where

import Good.Prelude

import qualified Data.Map.Strict as Map

import Data.Vector ((!?), (//))
--import qualified Data.Vector as Vector

import Control.Monad.State.Class

import Good.Architecture.Input
import Good.Architecture.Inputs.FSRead
import Good.Architecture.Output
import Good.Architecture.Outputs.FSWrite

import Good.Services.Lake.Utility.Error
import Good.Services.Lake.Utility.Coords
import Good.Services.Lake.Model.Item
import Good.Services.Lake.Model.Player
import Good.Services.Lake.Model.Entity
import Good.Services.Lake.Model.Chunk
import Good.Services.Lake.Model.Plane

data Universe = Universe { universeLoadedPlanes :: Map Text Plane -- map plane ids to planes
                         , universePlayerCoords :: Map Text GlobalCoords -- map player ids to their cells
                         }
  deriving Show

planePath :: Text -> Text
planePath pid = pid <> ".json"

universeEnsurePlaneCached :: (MonadIO m, MonadCatch m) => Universe -> Text -> m Universe
universeEnsurePlaneCached u pid =
  case lookup pid $ universeLoadedPlanes u of
    Just _ -> pure u
    Nothing -> do
      plane <- inputting planeStoreRead . getJSON . FSRead $ planePath pid
      pure $ u { universeLoadedPlanes = Map.insert pid plane $ universeLoadedPlanes u }

savePlane :: (MonadIO m, MonadCatch m) => Text -> Plane -> m ()
savePlane pid p = do
  forM_ (Map.toList $ planeLoadedChunks p) $ \(coords, chunk) -> saveChunk (pid, coords) chunk
  outputting planeStoreWrite $ putJSON (FSWrite $ planePath pid) p

universeInactivePlanes :: Universe -> [Text]
universeInactivePlanes u = inactive
  where inhabited :: [Text]
        inhabited = planeId <$> Map.elems (universePlayerCoords u)
        inactive :: [Text]
        inactive = filter (not . flip elem inhabited) . Map.keys $ universeLoadedPlanes u

save :: (MonadIO m, MonadCatch m, MonadState Universe m) => m ()
save = get >>= mapM_ (uncurry savePlane) . Map.toList . universeLoadedPlanes

cleanup :: (MonadIO m, MonadCatch m, MonadState Universe m) => m ()
cleanup = do
  u <- get
  let inhabited = planeId <$> Map.elems (universePlayerCoords u)
  m <- foldM
    ( \ps (pid, p) ->
        if pid `elem` inhabited
        then do
          p' <- cleanupPlane (chunkId <$> Map.elems (universePlayerCoords u)) pid p
          pure $ Map.update (const p') pid ps
        else savePlane pid p $> Map.delete pid ps
    )
    (universeLoadedPlanes u)
    (Map.toList $ universeLoadedPlanes u)
  put u { universeLoadedPlanes = m }

getPlane :: (MonadIO m, MonadCatch m, MonadState Universe m) => Text -> m Plane
getPlane pid = do
  u <- get
  u' <- universeEnsurePlaneCached u pid
  case lookup pid $ universeLoadedPlanes u' of
    Just chunk -> put u' $> chunk
    Nothing -> throwM . LakeError $ mconcat ["Plane ", pid, " was improperly cached"]

universeUpdatePlane :: Text -> (Plane -> Plane) -> Universe -> Universe
universeUpdatePlane pid f u = u { universeLoadedPlanes = Map.adjust f pid $ universeLoadedPlanes u }

updatePlane :: (MonadIO m, MonadCatch m, MonadState Universe m) => Text -> (Plane -> Plane) -> m ()
updatePlane pid f = do
  p <- getPlane pid
  modify . universeUpdatePlane pid . const $ f p

getChunk :: (MonadIO m, MonadCatch m, MonadState Universe m) => ChunkIdentifier -> m Chunk
getChunk cid@(pid, _) = do
  p <- getPlane pid
  (p', chunk) <- planeGetChunk p cid
  modify . universeUpdatePlane pid $ const p'
  pure chunk

updateChunk :: (MonadIO m, MonadCatch m, MonadState Universe m) => ChunkIdentifier -> (Chunk -> Chunk) -> m ()
updateChunk cid@(pid, _) f = do
  p <- getPlane pid
  (p', chunk) <- planeGetChunk p cid
  modify . universeUpdatePlane pid . const . planeUpdateChunk p' cid . const $ f chunk

getCell :: (MonadIO m, MonadCatch m, MonadState Universe m) => GlobalCoords -> m ChunkCell
getCell (cid, (x, y)) = do
  chunk <- getChunk cid
  case chunkCells chunk !? (y * chunkWidth + x) of
    Just cell -> pure cell
    Nothing -> throwM . LakeError $ mconcat ["Could not get cell at ", pack $ show cid]

updateCell :: (MonadIO m, MonadCatch m, MonadState Universe m) => GlobalCoords -> (ChunkCell -> ChunkCell) -> m ()
updateCell (cid, (x, y)) f = do
  chunk <- getChunk cid
  let i = y * chunkWidth + x
  case chunkCells chunk !? i of
    Just cell -> updateChunk cid . const $ chunk { chunkCells = chunkCells chunk // [(i, f cell)] }
    Nothing -> throwM . LakeError $ mconcat ["Could not update cell at ", pack $ show cid]

playerCoords :: (MonadIO m, MonadCatch m, MonadState Universe m) => Text -> m GlobalCoords
playerCoords player = do
  u <- get
  case Map.lookup player $ universePlayerCoords u of
    Nothing -> throwM . LakeError $ mconcat ["Player ", pack $ show player, " has no associated location"]
    Just c -> pure c

playerStats :: (MonadIO m, MonadCatch m, MonadState Universe m) => Text -> m PlayerStats
playerStats player = do
  coords <- playerCoords player
  cell <- getCell coords
  case chunkCellEntity cell of
    Nothing -> throwM . LakeError $ mconcat ["Location associated with player ", pack $ show player, " is incorrect"]
    Just e ->
      case entityPlayerStats e of
        Nothing -> throwM . LakeError $ mconcat ["Location associated with player ", pack $ show player, " is incorrect"]
        Just pstats -> pure pstats

moveEntity :: (MonadIO m, MonadCatch m, MonadState Universe m) => GlobalCoords -> GlobalCoords -> m ()
moveEntity s f = do
  scell <- getCell s
  fcell <- getCell s
  case (chunkCellEntity scell, chunkCellEntity fcell) of
    (Just e, Nothing) -> do
      updateCell s $ const scell { chunkCellEntity = Nothing }
      updateCell f $ const scell { chunkCellEntity = Just e }
      case entityPlayerStats e of
        Just pstats -> do
          u <- get
          put u { universePlayerCoords = Map.insert (playerStatsId pstats) f $ universePlayerCoords u }
        Nothing -> pure ()
    (Nothing, _) -> throwM . LakeError $ mconcat ["Attempted to move nonexistent entity at ", pack $ show s, " to ", pack $ show f]
    (_, Just _) -> throwM . LakeError $ mconcat ["Attempted to move entity at ", pack $ show s, " to occupied cell at ", pack $ show f]

move :: (MonadIO m, MonadCatch m, MonadState Universe m) => GlobalCoords -> Direction -> m ()
move c d = moveEntity c (along d c)

pickup :: (MonadIO m, MonadCatch m, MonadState Universe m) => GlobalCoords -> Text -> m ()
pickup c i = updateCell c $ \cell ->
  case chunkCellEntity cell of
    Nothing -> cell
    Just e -> case entityPlayerStats e of
      Nothing -> cell
      Just pstats -> case headMay . filter ((== i) . itemId) $ chunkCellItems cell of
        Nothing -> cell
        Just item -> cell { chunkCellEntity = Just e
                            { entityPlayerStats = Just pstats
                              { playerStatsInventory = item:playerStatsInventory pstats
                              }
                            }
                          , chunkCellItems = filter ((/= i) . itemId) $ chunkCellItems cell 
                          }

drop :: (MonadIO m, MonadCatch m, MonadState Universe m) => GlobalCoords -> Text -> m ()
drop c i = updateCell c $ \cell ->
  case chunkCellEntity cell of
    Nothing -> cell
    Just e -> case entityPlayerStats e of
      Nothing -> cell
      Just pstats -> case headMay . filter ((== i) . itemId) $ playerStatsInventory pstats of
        Nothing -> cell
        Just item -> cell { chunkCellEntity = Just e
                            { entityPlayerStats = Just pstats
                              { playerStatsInventory = filter ((/= i) . itemId) $ playerStatsInventory pstats 
                              }
                            }
                          , chunkCellItems = item:chunkCellItems cell
                          }
