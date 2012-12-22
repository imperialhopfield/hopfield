module Utils where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.List
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Test.QuickCheck
import           Control.Monad
import           Data.Number.Erf (normcdf)

import           Hopfield
import           Util

-- | Defines an arbitrary vector
instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary

nonempty = (`suchThat` (not . null))

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


patListGen :: Int -> Int -> Gen [Pattern]
patListGen maxPatSize maxPatListSize = do
    i <- choose (1, maxPatSize)
    nonempty $ boundedListGen (patternGen i) maxPatListSize


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
allWeightsSame :: Int -> [[Double]]
allWeightsSame n
  = [ [ if i==j then 0 else w | i <- [0..n-1] ] | j <- [0..n-1] ]
    where w = 1 ./. n


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


-- | Compute crosstalk term for a pattern and a given neuron
-- @crosstalk hopfield index neuron
-- todo think if it is better to actually pass in the hopfield data
-- strucutre
-- the pattern on which we do this has to be one of the traninig patterns
-- todo error checks
-- note that this is a very basic check
-- one should try and implement the probability error thing as
-- that would give as a good idea of how to
-- scale
crosstalk:: HopfieldData -> Int -> Int -> Int
-- the cross talk term is h(xi k ) - xi k
crosstalk hs index n = h (weights hs) pat n - pat V.! n
                          where pat = (patterns hs) !! index

compTerm:: HopfieldData -> Int -> Int -> Int
compTerm hs index n = - (pat V.! n) * (h (weights hs) pat n - pat V.! n)
                        where pat = (patterns hs) !! index

checkFixed :: HopfieldData -> Int -> Bool
checkFixed hs index = all (\x -> compTerm hs index x <= 1) [0.. V.length ((patterns hs) !! index) - 1]

-- | Used as a property to check that patterns which
-- are used to create the network are stable in respect to update
trainingPatsAreFixedPoints:: [Pattern] -> Gen Bool
trainingPatsAreFixedPoints pats =
  and <$> mapM checkFixedPoint [0.. length pats - 1]
  where
    hs = buildHopfieldData pats
    ws = weights hs
    checkFixedPoint index = do
      i <- arbitrary
      return $ evalRand (update ws (pats !! index)) (mkStdGen i) == (pats !! index) || (not $ checkFixed hs index)


-- | @compError hopfield@: Computes the percentage of patterns in the network
-- which are NOT fixed points
compError :: HopfieldData -> Double
compError hs = num_errors / (fromIntegral num_pats)
  where
    fixed_points = map (checkFixed hs) [0..num_pats-1]
    num_errors   = fromIntegral . length $ filter not fixed_points
    num_pats     = length $ patterns hs


-- | @compExpectedError hopfield@: Computes the expected error for a network
-- containing random iid patterns
compExpectedError :: HopfieldData -> Double
compExpectedError hs = normcdf x
  where
    variance = p2nRatio hs
    x        = -1 * ( sqrt (1 / variance) )


-- |@p2nRatio hopfield@: Computes the ratio p/n, the number of patterns to
-- the number of neurons
p2nRatio :: HopfieldData -> Double
p2nRatio hs = num_pats / num_neurons
  where
    num_pats    = fromIntegral . length $ patterns hs
    num_neurons = fromIntegral . V.length $ (patterns hs) !! 0

-- | Trains a network using @training_pats@ and then updates each
-- pattern in pats according to the weights of that network.
-- The aim is to check that the energy decreases after each update.
energyDecreasesAfterUpdate:: ([Pattern], [Pattern]) -> Gen Bool
energyDecreasesAfterUpdate (training_pats, pats)
  = and <$> mapM energyDecreases pats
    where
      ws = weights $ buildHopfieldData training_pats
      energyDecreases' pat = do
        pattern_after_update <- update ws pat
        return (energy ws pat >= energy ws pattern_after_update || energy ws pattern_after_update - energy ws pat <= 0.00000001 )
      energyDecreases pat = do
        i <- arbitrary
        return $ evalRand (energyDecreases' pat) (mkStdGen i)


repeatedUpdateCheck:: ([Pattern], [Pattern]) -> Gen Bool
repeatedUpdateCheck (training_pats, pats)
  = and <$> mapM  s pats
    where
      ws = weights $ buildHopfieldData training_pats
      stopped pat = do
        p     <- converged_pattern
        new_p <- update ws p
        return $ p == new_p
        where
          converged_pattern = repeatedUpdate ws pat
      s pat = do
        i <- arbitrary
        return $ evalRand (stopped pat) (mkStdGen i)
