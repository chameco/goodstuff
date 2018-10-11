module Good.Architecture.Inputs.PostgreSQLRead where

import Good.Prelude

import qualified Database.PostgreSQL.Simple as DB

import Good.Architecture.Input

newtype PostgreSQLReadError = PostgreSQLReadError Text deriving Show
instance Exception PostgreSQLReadError

data PostgreSQLRead = PostgreSQLRead { query :: DB.Query, subs :: [ByteString]}

instance Input PostgreSQLRead where
  data InputConfig PostgreSQLRead = PostgreSQLReadConfig { conn :: DB.Connection }
  data InputState PostgreSQLRead = PostgreSQLReadState
  initialInputState = PostgreSQLReadState
  getRaw i = do c <- ask
                res <- liftIO . DB.withTransaction (conn c) $ DB.query (conn c) (query i) (subs i)
                case res of ((x:_):_) -> pure x
                            _ -> throwM . PostgreSQLReadError $ mconcat ["Query \"", tshow (query i), "\" returned no results"]
