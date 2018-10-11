module Good.Services.Booru where

import Good.Prelude

import qualified Database.SQLite.Simple as DB

import Good.Architecture.Input
import qualified Good.Architecture.Inputs.FSRead as FSRead
import Good.Architecture.Inputs.TagStoreRead

import Good.Architecture.Output
import qualified Good.Architecture.Outputs.FSWrite as FSWrite
import Good.Architecture.Outputs.TagStoreWrite

import Good.Interfaces.Web

api :: Serving IO ()
api = do
  handling (Get "/booru") $ do
    ts <- tags
    case ts of
      [] -> pure $ Plaintext "Search for files by specifying tags in the query string.\n"
      _ -> do
        conn <- db
        tagged <- inputting (TagStoreReadConfig (FSRead.FSReadConfig store) conn) $ getTagged (TagStoreRead ts)
        pure . Plaintext . mconcat $ ("/booru/"<>) . (<>"\n") . FSRead.fsPath <$> tagged
  handling (Post "/booru") $ do
    ts <- tags
    fs <- files
    case fs of [File _ _ _ file] -> do
                 conn <- db
                 outputting (TagStoreWriteConfig (FSWrite.FSWriteConfig store) conn) $
                   putRaw (TagStoreWrite ts) file
                 pure $ Plaintext "Upload successful\n"
               _ -> throwM (WebError status400 "Must send exactly one file\n")
  handling (Put "/booru/:path") $ do
    path <- param "path"
    ts <- tags
    conn <- db
    outputting (TagStoreWriteConfig (FSWrite.FSWriteConfig store) conn) $
      addTags (TagStoreWrite ts) (FSWrite.FSWrite path)
    pure $ Plaintext "Tags added successfully\n"
  where tags = fmap snd . filter ((=="tag") . fst) <$> params
        db = liftIO $ DB.open "db/booru.sqlite3"
        store = "store/booru"
