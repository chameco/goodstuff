module Good.Architecture.Outputs.TagStoreWrite where

import Good.Prelude

import Data.Digest.Pure.MD5 (md5)

import qualified Database.SQLite.Simple as DB

import Good.Architecture.Output
import Good.Architecture.Outputs.FSWrite

newtype TagStoreWriteError = TagStoreWriteError Text deriving Show
instance Exception TagStoreWriteError

newtype TagStoreWrite = TagStoreWrite { tags :: [Text]}

ensureTagTables :: MonadIO m => Outputting TagStoreWrite m ()
ensureTagTables = do c <- ask
                     void . liftIO . DB.withTransaction (conn c) $ do
                       DB.execute_ (conn c) "create table if not exists files (file_id integer primary key, file_path text unique)"
                       DB.execute_ (conn c) "create table if not exists tags (tag_id integer primary key, tag_name text unique)"
                       DB.execute_ (conn c) "create table if not exists file_tags (file_id integer, tag_id integer)"

addTags :: MonadIO m => TagStoreWrite -> FSWrite -> Outputting TagStoreWrite m ()
addTags o (FSWrite path) = do
  c <- ask
  void . liftIO . DB.withTransaction (conn c) $ do
    res_file <- DB.query (conn c) "select (file_id) from files where file_path = ?" (DB.Only path)
    case res_file of
      [] -> throwM . TagStoreWriteError $ mconcat ["File at \"", path, "\" does not exist"]
      (DB.Only file_id:_) ->
        forM_ (tags o) $
          \tag -> do
            res_tag <- DB.query (conn c) "select (tag_id) from tags where tag_name = ?" (DB.Only tag)
            tag_id <- case res_tag of (DB.Only tag_id:_) -> pure tag_id
                                      [] -> do DB.execute (conn c) "insert into tags (tag_name) values (?)" (DB.Only tag)
                                               DB.lastInsertRowId (conn c)
            res <- DB.query (conn c) "select * from file_tags where file_id = ? and tag_id = ?" (file_id :: Int64, tag_id :: Int64)
            case res of [] -> DB.execute (conn c) "insert into file_tags (file_id, tag_id) values (?, ?)" (file_id, tag_id)
                        ((_ :: Int64, _ :: Int64):_) -> pure ()

instance Output TagStoreWrite where
  data OutputConfig TagStoreWrite = TagStoreWriteConfig { fs :: OutputConfig FSWrite, conn :: DB.Connection }
  data OutputState TagStoreWrite = TagStoreWriteState
  initialOutputState = TagStoreWriteState
  putRaw o d = do c <- ask
                  let path = tshow . md5 $ toSL d
                  outputting (fs c) $ putRaw (FSWrite path) d
                  ensureTagTables
                  void . liftIO . DB.withTransaction (conn c) $ do
                    res_file <- DB.query (conn c) "select (file_id) from files where file_path = ?" (DB.Only path)
                    file_id <- case res_file of (DB.Only file_id:_) -> pure file_id
                                                [] -> do DB.execute (conn c) "insert into files (file_path) values (?)" (DB.Only path)
                                                         DB.lastInsertRowId (conn c)
                    forM_ (tags o) $
                      \tag -> do
                        res_tag <- DB.query (conn c) "select (tag_id) from tags where tag_name = ?" (DB.Only tag)
                        tag_id <- case res_tag of (DB.Only tag_id:_) -> pure tag_id
                                                  [] -> do DB.execute (conn c) "insert into tags (tag_name) values (?)" (DB.Only tag)
                                                           DB.lastInsertRowId (conn c)
                        res <- DB.query (conn c) "select * from file_tags where file_id = ? and tag_id = ?" (file_id, tag_id)
                        case res of [] -> DB.execute (conn c) "insert into file_tags (file_id, tag_id) values (?, ?)" (file_id, tag_id)
                                    ((_ :: Int64, _ :: Int64):_) -> pure ()
