module Good.Services.Host where

import Good.Prelude

import Good.Architecture.Inputs.FSRead
import Good.Architecture.Output
import Good.Architecture.Outputs.FSWrite
import Good.Interfaces.Web

api :: Serving IO ()
api = do
  handling (Get "/host") . pure $ Plaintext "This is a file hosting server.\n"
  handling (Post "/host/:path") $ do
    path <- param "path"
    fs <- files
    case fs of [File _ _ _ file] -> do
                 liftIO $ outputting (FSWriteConfig "store/host") (putRaw (FSWrite path) file)
                 pure $ Plaintext "Upload successful\n"
               _ -> throwM (WebError status400 "Must send exactly one file.\n")
  handling (Get "/host/:path") $ do
    path <- param "path"
    pure $ FS (FSReadConfig "store/host") (FSRead path)
