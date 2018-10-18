module Good.Architecture.Inputs.SQLiteRead where

import Good.Prelude

import qualified Database.SQLite.Simple as DB

import Good.Architecture.Input

newtype SQLiteReadError = SQLiteReadError Text deriving Show
instance Exception SQLiteReadError

data SQLiteRead = SQLiteRead { query :: DB.Query, subs :: [DB.NamedParam]}

instance Input SQLiteRead where
  data InputConfig SQLiteRead = SQLiteReadConfig { conn :: DB.Connection }
  data InputState SQLiteRead = SQLiteReadState
  initialInputState = SQLiteReadState
  getRaw i = do c <- ask
                res <- liftIO . DB.withTransaction (conn c) $ DB.queryNamed (conn c) (query i) (subs i)
                case res of ((x:_):_) -> pure x
                            _ -> throwM . SQLiteReadError $ mconcat ["Query \"", tshow (query i), "\" returned no results"]
