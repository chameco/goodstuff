module Good.Architecture.Pipeline where

import Good.Prelude

import Data.Aeson (FromJSON, ToJSON)

import Good.Architecture.Input
import Good.Architecture.Output

type Pipeline m i o = (MonadIO m, MonadCatch m) => Outputting o (Inputting i m) ()

piping :: (Input i, Output o, MonadIO m, MonadCatch m) => InputConfig i -> OutputConfig o -> Pipeline m i o -> m ()
piping ic oc p = inputting ic $ outputting oc p

rawpipe :: (Input i, Output o) => (ByteString -> ByteString) -> i -> o -> Pipeline m i o
rawpipe f i o = lift (getRaw i) >>= putRaw o . f

rawpipeM :: (Input i, Output o) => (ByteString -> m ByteString) -> i -> o -> Pipeline m i o
rawpipeM f i o = lift (getRaw i) >>= lift . lift . f >>= putRaw o

pipe :: (Input i, Output o, FromJSON a, ToJSON b) => (a -> b) -> i -> o -> Pipeline m i o
pipe f i o = lift (getJSON i) >>= putJSON o . f

pipeM :: (Input i, Output o, FromJSON a, ToJSON b) => (a -> m b) -> i -> o -> Pipeline m i o
pipeM f i o = lift (getJSON i) >>= lift . lift . f >>= putJSON o
