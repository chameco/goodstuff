module Good.Architecture.Input where

import Good.Prelude

import Data.Aeson (FromJSON, eitherDecode)

import Control.Monad.State (StateT, MonadState, runStateT)

import Good.Architecture.Error

class Input i where
    data InputConfig i :: * 
    data InputState i :: *
    initialInputState :: InputState i
    getRaw :: (MonadIO m, MonadThrow m) => i -> Inputting i m ByteString
    getJSON :: (FromJSON a, MonadIO m, MonadThrow m) => i -> Inputting i m a
    getJSON i = (eitherDecode . fromStrict <$> getRaw i) >>= throwLeft (DecodeError . toSL)

newtype Inputting i m a = Inputting { runInputting :: ReaderT (InputConfig i) (StateT (InputState i) m) a }
                                    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (InputConfig i), MonadState (InputState i), MonadThrow)

instance MonadTrans (Inputting i) where
    lift = Inputting . lift . lift

inputting :: (Input i, MonadIO m) => InputConfig i -> Inputting i m a -> m a
inputting c b = fst <$> runStateT (runReaderT (runInputting b) c) initialInputState

inputtingState :: (Input i, MonadIO m) => InputConfig i -> InputState i -> Inputting i m a -> m a
inputtingState c s b = fst <$> runStateT (runReaderT (runInputting b) c) s
