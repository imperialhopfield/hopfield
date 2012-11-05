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


-- | @boundedListGen g n@: Generates lists (max length n) of the given Gen
boundedListGen :: Gen a -> Int -> Gen [a]
boundedListGen g n = do
  len <- choose (0, n)
  vectorOf len g

-- TODO add description and add non-empty
patListGen maxPatSize maxPatListSize = do
    i <- choose (1, maxPatSize)
    (`suchThat` (not . null)) $ boundedListGen (patternGen i) maxPatListSize


-- | @patternsTupleGen g m1 m2@Generates a tuple of lists, as follows:
-- Uses patListGen to generate 1 list of patterns with length less than m2.
-- The list itself has to have length less than m1.
-- The second element of a tuple is a list of patterns which have the same size
-- as the patterns of the first list.
patternsTupleGen :: Int -> Int -> Gen ([Pattern], [Pattern])
patternsTupleGen m1 m2 = do
  fst_list <- patListGen  m1 m2
  i <- choose (0, m2)
  snd_list <- vectorOf i (patternGen $ V.length $ head fst_list)
  return $ (fst_list, snd_list)


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


-- | @boundedReplicateGen n g@ Generates lists containing 'g' replicated.
-- The list is bounded in size by n.
boundedReplicateGen :: Int -> Gen a -> Gen [a]
boundedReplicateGen n g = liftM2 replicate (choose (0, n)) g


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

-- | Tranins a network using @traning_pats@ and then updates each
-- pattern in pats according to the weigths of that network.
-- The aim is to check that the energy decreases after each update.
energyDecreasesAfterUpdate:: ([Pattern], [Pattern]) -> Gen Bool
energyDecreasesAfterUpdate (training_pats, pats)
  = and <$> mapM energyDecreases pats
    where
      ws = weights $ buildHopfieldData training_pats
      energyDecreases' pat = do
        pattern_after_update <- update ws pat
        return (energy ws pat >= energy ws pattern_after_update)
      energyDecreases pat = do
        i<- arbitrary
        return $ evalRand (energyDecreases' pat) (mkStdGen i)
