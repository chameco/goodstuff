module Good.Architecture.Outputs.SQLiteWrite where

import Good.Prelude

import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple (NamedParam((:=)))

import Good.Architecture.Output

newtype SQLiteWriteError = SQLiteWriteError Text deriving Show
instance Exception SQLiteWriteError

data SQLiteWrite = SQLiteWrite { query :: DB.Query, subs :: [DB.NamedParam]}

instance Output SQLiteWrite where
  data OutputConfig SQLiteWrite = SQLiteWriteConfig { conn :: DB.Connection }
  data OutputState SQLiteWrite = SQLiteWriteState
  initialOutputState = SQLiteWriteState
  putRaw i d = do c <- ask
                  liftIO . DB.withTransaction (conn c) . DB.executeNamed (conn c) (query i) $ (":data" := d):subs i
