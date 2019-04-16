module Good.Architecture.Output where

import Good.Prelude

import Data.Aeson (ToJSON, encode)

class Output o where
  data OutputConfig o :: Type
  data OutputState o :: Type
  initialOutputState :: OutputState o
  putRaw :: (MonadIO m, MonadCatch m) => o -> ByteString -> Outputting o m ()
  putJSON :: (ToJSON a, MonadIO m, MonadCatch m) => o -> a -> Outputting o m ()
  putJSON o = putRaw o . encode

newtype Outputting o m a = Outputting { runOutputting :: ReaderT (OutputConfig o) (StateT (OutputState o) m) a }
                         deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (OutputConfig o), MonadState (OutputState o), MonadThrow, MonadCatch)

instance MonadTrans (Outputting o) where
  lift = Outputting . lift . lift

outputting :: (Output o, MonadIO m) => OutputConfig o -> Outputting o m a -> m a
outputting c b = fst <$> runStateT (runReaderT (runOutputting b) c) initialOutputState

outputtingState :: (Output o, MonadIO m) => OutputConfig o -> OutputState o -> Outputting o m a -> m a
outputtingState c s b = fst <$> runStateT (runReaderT (runOutputting b) c) s
