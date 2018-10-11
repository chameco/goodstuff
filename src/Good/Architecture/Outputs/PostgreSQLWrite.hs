module Good.Architecture.Outputs.PostgreSQLWrite where

import Good.Prelude

import qualified Database.PostgreSQL.Simple as DB

import Good.Architecture.Output

newtype PostgreSQLWriteError = PostgreSQLWriteError Text deriving Show
instance Exception PostgreSQLWriteError

data PostgreSQLWrite = PostgreSQLWrite { query :: DB.Query, subs :: [ByteString]}

instance Output PostgreSQLWrite where
  data OutputConfig PostgreSQLWrite = PostgreSQLWriteConfig { conn :: DB.Connection }
  data OutputState PostgreSQLWrite = PostgreSQLWriteState
  initialOutputState = PostgreSQLWriteState
  putRaw i d = do c <- ask
                  void . liftIO . DB.withTransaction (conn c) . DB.execute (conn c) (query i) $ d:subs i
