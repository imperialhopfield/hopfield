{-# LANGUAGE ParallelListComp #-}


module Util (
    combine
  , findInList
  , fromDataVector
  , repeatUntilEqual
  , repeatUntilEqualOrLimitExceeded
  , randomElem
  , vector2D
  , list2D
  , toBinary
  , getBinaryIndices
  , (./.)
  , (*.)
) where

import           Data.List
import           Data.Maybe
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


repeatUntilEqualOrLimitExceeded :: (MonadRandom m, Eq a) => Int -> (a -> m a) -> a -> m a
repeatUntilEqualOrLimitExceeded limit f a
  | limit < 0 = error "negative limit in repeatUntilEqualOrLimitExceeded"
  | otherwise = repeatUntilEqualOrLimitExceeded' 0 limit f a


repeatUntilEqualOrLimitExceeded' :: (MonadRandom m, Eq a) => Int -> Int -> (a -> m a) -> a -> m a
repeatUntilEqualOrLimitExceeded' current limit f a
  | current == limit = return a
  | otherwise = do
      new_a <- f a
      if a == new_a then return a else repeatUntilEqualOrLimitExceeded' (current + 1) limit f new_a


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

-- Tries to find a element in a list. In case of success, returns the index
-- of the element (the first one, in case of multiple occurences). In case of
-- failure, returns the search element itself.
findInList :: Eq a => [a] -> a -> Either a Int
findInList xs x =
  case m_index of
        Nothing    -> Left x
        Just index -> Right index
  where m_index = x `elemIndex` xs


-- @toBinary n size@. Returns the binary representation of n in size bits.
-- The caller has to ensure that n fits in size bits, or an error will be raised.
toBinary :: Int -> Int -> [Int]
toBinary n size
  | n < 0             = error "toBinary requires positive arguments"
  | n >  2 ^ size - 1 = error "cannot fit binary representation into given size"
  | otherwise =  [ (n `div` 2 ^ i) `mod` 2 | i <- [size - 1, size - 2 .. 0] ]


-- TODO write comment
getBinaryIndices :: Eq a => [a] -> [(a, [Int])]
getBinaryIndices xs = [ (x, toBinary i bitsNeeded) | i <- [0 ..] | x <- nub_xs]
  where
    nub_xs = nub xs
    bitsNeeded = ceiling $ logBase 2.0 $ fromIntegral (length nub_xs)
