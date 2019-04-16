module Good.Interfaces.Log where

import Good.Prelude

import System.IO

import Data.Text.Lazy (unpack)

notify :: MonadIO m => Text -> m ()
notify = liftIO . hPutStrLn stderr . unpack
