module Good.Prelude
  ( Integer, rem, (^), pred, succ

  , Num(..)
  , Generic(..)

  , IO, stdin, stdout, stderr, FilePath, (</>)

  , Type
  , const, flip, ($), (.), (|>)
  , fst, snd, curry, uncurry
  , Maybe(..), maybe, fromMaybe, isJust, catMaybes
  , Either(..)
  , take, drop, dropWhile, filter, reverse, lookup, zip, replicate, sortOn, concatMap, elemIndex, headMay, atMay
  , Bool(..), not, otherwise, (&&), (||)
  , Char
  , Int, Int64
  , String
  , Text, pack, unpack, tshow, putStrLn
  , ByteString, fromStrict, toStrict, readFile, writeFile
  , UTCTime(..), getCurrentTime
  , toSL
  , Eq(..)
  , Ord(..), Down(..)
  , Semigroup(..)
  , Monoid(..)
  , Functor(..), void, (<$>), ($>)
  , Bifunctor(..)
  , Traversable(..), forM
  , Foldable(..), any, all, mapM_, forM_

  , Show (..)
  , readMaybe

  , Applicative(..), (<|>)
  , Monad(..), join, forever, foldM, (>=>)
  , MonadIO(..)
  , ReaderT(..)
  , MonadReader(..)
  , StateT(..)
  , MonadState(..)
  , MonadTrans(..)
  , (>>>)
  , Exception, SomeException, IOException, MonadThrow, MonadCatch, throwM, try, catch, throwLeft
  ) where

import Prelude (Integer, rem, (^), pred, succ)

import GHC.Num (Num(..))
import GHC.Generics (Generic(..))

import System.IO (IO, stdin, stdout, stderr, FilePath)
import System.FilePath.Posix ((</>))

import Data.Kind (Type)
import Data.Function (const, flip, ($), (.))
import Data.Tuple (fst, snd, curry, uncurry)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust, catMaybes)
import Data.Either (Either(..))
import Data.List (take, drop, dropWhile, filter, reverse, lookup, zip, replicate, sortOn, concatMap, elemIndex)
import Data.Bool (Bool(..), not, otherwise, (&&), (||))
import Data.Char (Char)
import Data.Int (Int, Int64)
import Data.String (String)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Lazy.IO (putStrLn)
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict, readFile, writeFile)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.String.Conv (toSL)
import Data.Eq (Eq(..))
import Data.Ord (Ord(..), Down(..))
import Data.Semigroup (Semigroup(..), (<>))
import Data.Monoid (Monoid(..))
import Data.Functor (Functor(..), void, (<$>), ($>))
import Data.Bifunctor (Bifunctor(..))
import Data.Traversable (Traversable(..), forM)
import Data.Foldable (Foldable(..), any, all, mapM_, forM_)

import Text.Show (Show(..))
import Text.Read (readMaybe)

import Control.Applicative (Applicative(..), (<|>))
import Control.Monad (Monad(..), join, forever, foldM, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State (StateT(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Arrow ((>>>))
import Control.Exception.Safe (Exception, SomeException, IOException, MonadThrow, MonadCatch, throwM, try, catch)

infixl |>
(|>) :: a -> (a -> b) -> b
a |> f = f a

tshow :: Show a => a -> Text
tshow = pack . show

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

atMay :: [a] -> Int -> Maybe a
atMay [] _ = Nothing
atMay (x:_) 0 = Just x
atMay (_:xs) n = atMay xs $ n - 1

throwLeft :: (Exception e, MonadThrow m) => (b -> e) -> Either b a -> m a
throwLeft f (Left x) = throwM $ f x
throwLeft _ (Right x) = pure x
