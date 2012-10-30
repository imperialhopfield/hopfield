module Utils where

import qualified Data.Vector as V
import           Test.QuickCheck
import           Control.Monad
import           Test.QuickCheck
import           Control.Monad
import           Hopfield

import           Util

-- | Defines an arbitrary vector
instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = liftM V.fromList arbitrary


-- | Convert a list generator to a vector generator
toGenVector :: Gen [a] -> Gen (V.Vector a)
toGenVector listGen = liftM V.fromList listGen


-- | @patternGen n@: Generates patterns of size n
patternGen :: Int -> Gen Pattern
patternGen n = toGenVector $ vectorOf n arbitrary


-- | @boundedListGen g n@: Generates lists (max length n) of the given Gen
boundedListGen :: Gen a -> Int -> Gen [a]
boundedListGen g n = do
  len <- choose (0, n)
  vectorOf len g


-- Generate lists containing only 'n'
sameElemList :: a -> Gen [a]
sameElemList x = do
  len <- arbitrary
  return $ replicate len x


-- | Generate vectors containing the same element replicated
sameElemVector :: a -> Gen (V.Vector a)
sameElemVector = toGenVector . sameElemList


-- | Produces a matrix with 0's along the diagonal and 1's otherwise
allOnesWeights :: Int -> [[Double]]
allOnesWeights n
  = [ [ if i==j then 0 else 1 | i <- [1..n-1] ] | j <- [1..n-1] ]


boundedClonedGen :: Int -> Gen a -> Gen [a]
boundedClonedGen n g = liftM2 replicate (choose (0, n)) g


-- | Replaces the nth element in the list with 'r'
replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ []     = error "index greater than list size"
replaceAtN 0 r (x:xs) = (r:xs)
replaceAtN n r (x:xs)
  | n > 0     = (x:(replaceAtN (n-1) r xs))
  | otherwise = error "negative index"
