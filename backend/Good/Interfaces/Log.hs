module Good.Interfaces.Log where

import Good.Prelude

import System.IO

notify :: MonadIO m => Text -> m ()
notify = liftIO . hPutStrLn stderr . unpack
