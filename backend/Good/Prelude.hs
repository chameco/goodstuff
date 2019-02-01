module Good.Prelude
  ( module ClassyPrelude
  , module Data.Kind
  , module Data.String.Conv
  , (>>>)
  , MonadThrow, MonadCatch, throwM, catch, throwLeft
  , (|>)
  ) where

import ClassyPrelude hiding (catch)

import Data.Kind
import Data.String.Conv

import Control.Arrow ((>>>))

import Control.Exception.Safe (MonadThrow, MonadCatch, throwM, catch)

throwLeft :: (Exception e, MonadThrow m) => (b -> e) -> Either b a -> m a
throwLeft f (Left x) = throwM $ f x
throwLeft _ (Right x) = pure x

infixl |>
(|>) :: a -> (a -> b) -> b
a |> f = f a
