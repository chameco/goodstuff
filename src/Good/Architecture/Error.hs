module Good.Architecture.Error where

import Good.Prelude

newtype DecodeError = DecodeError Text deriving Show
instance Exception DecodeError

throwLeft :: (Exception e, MonadThrow m) => (b -> e) -> Either b a -> m a
throwLeft f (Left x) = throwM $ f x
throwLeft _ (Right x) = pure x
