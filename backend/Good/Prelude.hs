module Good.Prelude
  ( module ClassyPrelude
  , module Data.Kind
  , module Data.String.Conv
  , (>>>)
  , MonadThrow, MonadCatch, throwM, try, catch, throwLeft
  , (|>)
  , atMay
  ) where

import ClassyPrelude hiding (try, catch)

import Data.Kind
import Data.String.Conv

import Control.Arrow ((>>>))

import Control.Exception.Safe (MonadThrow, MonadCatch, throwM, try, catch)

throwLeft :: (Exception e, MonadThrow m) => (b -> e) -> Either b a -> m a
throwLeft f (Left x) = throwM $ f x
throwLeft _ (Right x) = pure x

infixl |>
(|>) :: a -> (a -> b) -> b
a |> f = f a

atMay :: [a] -> Int -> Maybe a
atMay [] _ = Nothing
atMay (x:_) 0 = Just x
atMay (_:xs) n = atMay xs $ n - 1
