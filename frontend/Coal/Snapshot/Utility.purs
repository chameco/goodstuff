module Coal.Snapshot.Utility where

import Control.Semigroupoid ((<<<))
import Data.Boolean (otherwise)
import Data.Foldable (foldl)
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Sequence (Seq, empty, null, singleton, splitAt)
import Data.Tuple (Tuple(..), snd)

splitEvery :: forall a. Int -> Seq a -> Seq (Seq a)
splitEvery i s
 | null s = empty
 | otherwise = case splitAt i s of
   Tuple h t -> singleton h <> splitEvery i t

reverse :: forall a. Seq a -> Seq a
reverse = foldl (\xs x -> singleton x <> xs) empty

enumerate :: forall a. Int -> Seq a -> Seq (Tuple Int a)
enumerate i = reverse <<< snd <<< foldl (\(Tuple n xs) x -> Tuple (n + 1) $ singleton (Tuple n x) <> xs) (Tuple i empty)
