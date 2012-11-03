module Utils where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import qualified Data.Vector as V
import           Test.QuickCheck
import           Control.Monad

import           Hopfield
import           Util

-- | Defines an arbitrary vector
instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary


mapMonad :: Monad m => (a -> b) -> m [a] -> m [b]
mapMonad f m_xs = do
  xs <- m_xs
  return $ map f xs


-- | Convert a list generator to a vector generator
toGenVector :: Gen [a] -> Gen (V.Vector a)
toGenVector listGen = fmap V.fromList listGen


-- | Generate a random sign (+/- 1)
signGen :: Gen Int
signGen = do
  n <- choose (0,1)
  return $ n*2 - 1


-- | @patternGen n@: Generates patterns of size n
patternGen :: Int -> Gen Pattern
patternGen n = toGenVector $ vectorOf n signGen


-- | @boundedReplicateGen g n@: Generates lists (max length n) of the given Gen
boundedReplicateGen :: Gen a -> Int -> Gen [a]
boundedReplicateGen g n = do
  len <- choose (0, n)
  vectorOf len g


-- Generate lists containing only 'x'
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
  = [ [ if i==j then 0 else 1 | i <- [0..n-1] ] | j <- [0..n-1] ]


-- | @boundedClonedGen n g@ Generates lists containing 'g' replicated.
-- The list is bounded in size by n.
boundedClonedGen :: Int -> Gen a -> Gen [a]
boundedClonedGen n g = liftM2 replicate (choose (0, n)) g


-- | Replaces the nth element in the list with 'r'
replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ []     = error "index greater than list size"
replaceAtN 0 r (x:xs) = (r:xs)
replaceAtN n r (x:xs)
  | n > 0     = (x:(replaceAtN (n-1) r xs))
  | otherwise = error "negative index"


-- | Used as a property to check that patterns which
-- are used to create the network are stable in respect to update
trainingPatsAreFixedPoints:: [Pattern] -> Gen Bool
trainingPatsAreFixedPoints pats =
  and <$> mapM checkFixedPoint pats
  where
    ws = weights (buildHopfieldData pats)
    checkFixedPoint pat = do
      i <- arbitrary
      return $ evalRand (update ws pat) (mkStdGen i) == pat
