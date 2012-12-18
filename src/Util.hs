module Util (
    combine
  , fromDataVector
  , repeatUntilEqual
  , randomElem
  , vector2D
  , list2D
  , (./.)
  , (*.)
) where

import qualified Data.Vector as V
import           Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Random
import           Foreign.Storable
import qualified Numeric.Container as NC


(./.) :: (Fractional a, Integral a1, Integral a2) => a1 -> a2 -> a
x ./. y = fromIntegral x / fromIntegral y

(*.) :: (Integral a1, Num a) => a -> a1 -> a
x *. y = x * fromIntegral y


randomElem :: MonadRandom m => [a] -> m a
randomElem [] = error "randomElem: empty list"
randomElem xs = Random.fromList (zip xs (repeat 1))


repeatUntilEqual ::  (MonadRandom m, Eq a) => (a -> m a) -> a -> m a
repeatUntilEqual f a =
  do
    new_a <- f a
    if a == new_a then return a else repeatUntilEqual f new_a


-- | Converts a list of lists to a 2D vector
vector2D :: [[a]] -> V.Vector (V.Vector a)
vector2D ll = V.fromList $ map V.fromList ll


-- | Converts a 2D vector into a list of lists
list2D :: V.Vector (V.Vector a) -> [[a]]
list2D vv = map V.toList $ V.toList vv

-- from Data.Vector to Numeric.Container.Vector
fromDataVector::  (Foreign.Storable.Storable a) => V.Vector a -> NC.Vector a
fromDataVector v = NC.fromList $ V.toList v

-- the caller has to ensure that the dimensions are the same
combine:: (a-> a -> a) -> [[a]] -> [[a]] -> [[a]]
combine f xs ys = zipWith (zipWith f) xs ys