module Good.Services.Host where

import Good.Prelude

import Good.Architecture.Input
import Good.Architecture.Inputs.FSRead
import Good.Architecture.Output
import Good.Architecture.Outputs.FSWrite
import Good.Interfaces.Web

api :: Serving IO ()
api = do
  handling (Get "/host") . pure $ Plaintext "This is a file hosting server."
  handling (Post "/host/:path") $ do
    path <- param "path"
    ps <- files
    case ps of [File _ _ _ file] -> do
                 liftIO . putSafe path $ toSL file
                 pure $ Plaintext "Upload successful"
               _ -> throwM (WebError status400 "Must send exactly one file")
  handling (Get "/host/:path") $ do
    path <- param "path"
    file <- liftIO $ getSafe path
    case file of Nothing -> throwM (WebError status404 "File not found")
                 Just f -> pure $ Raw f

getSafe :: Text -> IO (Maybe ByteString)
getSafe path = catch
  (Just <$> inputting (FSReadConfig "store/host") (getRaw (FSRead path)))
  (const (pure Nothing) :: IOException -> IO (Maybe ByteString))

putSafe :: Text -> ByteString -> IO ()
putSafe path file = outputting (FSWriteConfig "store/host") (putRaw (FSWrite path) file)
