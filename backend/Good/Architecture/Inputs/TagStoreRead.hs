module Good.Architecture.Inputs.TagStoreRead where

import Good.Prelude

import qualified Database.SQLite.Simple as DB

import Good.Architecture.Input
import Good.Architecture.Inputs.FSRead

newtype TagStoreReadError = TagStoreReadError Text deriving Show
instance Exception TagStoreReadError

newtype TagStoreRead = TagStoreRead { tags :: [Text]}

getTagged :: (MonadIO m, MonadCatch m) => TagStoreRead -> Inputting TagStoreRead m [FSRead]
getTagged i = do c <- ask
                 catch (fmap (fmap FSRead . join) . liftIO . DB.withTransaction (conn c) $
                        DB.query (conn c) (DB.Query . toSL $ "select distinct file_path from files inner join file_tags on file_tags.file_id = files.file_id inner join tags on tags.tag_id = file_tags.tag_id where tags.tag_name in (" <> querysubs (tags i) <> ") group by file_path having count(file_path) = " <> tshow (length $ tags i)) (tags i))
                   (\(x :: DB.SQLError) -> case DB.sqlError x of DB.ErrorError -> pure []
                                                                 _ -> throwM x)
  where querysubs :: [a] -> Text
        querysubs [] = ""
        querysubs l = foldr (const ("?, "<>)) "?" $ drop 1 l

instance Input TagStoreRead where
  data InputConfig TagStoreRead = TagStoreReadConfig { fs :: InputConfig FSRead, conn :: DB.Connection }
  data InputState TagStoreRead = TagStoreReadState
  initialInputState = TagStoreReadState
  getRaw i = do c <- ask
                res <- getTagged i
                case res of (x:_) -> inputting (fs c) $ getRaw x
                            _ -> throwM . TagStoreReadError $ mconcat ["Tag search for \"", tshow (tags i), "\" returned no results"]
