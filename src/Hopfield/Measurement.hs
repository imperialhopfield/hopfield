-- | Functions to measure various properties of a network
module Hopfield.Measurement (
  -- * Basin of attraction
    BasinMeasure
  , hammingDistribution
  , sampleHammingRange
  , sampleHammingDistance
  , samplePatternRing
  , samplePatternBasin
  , measurePatternBasin
  -- * Fixed point errors
  , checkFixed
  , measureError
) where

import           Control.Monad (liftM, replicateM)
import           Control.Monad.Random (MonadRandom)
import           Data.List
import           Data.Maybe
import qualified Data.Vector as V
import           Math.Combinatorics.Exact.Binomial (choose)
import           Numeric.Probability.Distribution (Spread, relative)
import           Numeric.Probability.Random (T, pick)

import           Hopfield.Hopfield
import           Hopfield.Util ((./.), toArray, shuffle, runT)


-- A function computing some measure of a pattern's basin in the given network
type BasinMeasure m a = HopfieldData -> Pattern -> m a


-- -----------------------------------------------------------------------------
-- Functions relating to measuring a pattern's basin of attraction


-- Create a probability distribution for Hamming distances in the given range
hammingDistribution :: Int -> (Int, Int) -> T Int
hammingDistribution n (mini, maxi) = pick $ dist rs
  where
    dist  = relative probs :: Spread Double Int
    probs = [ fromIntegral $ n `choose` r | r <- rs]
    rs    = [mini..maxi]


-- Sample a pattern in the Hamming distance range specified by dist
sampleHammingRange :: MonadRandom m => Pattern -> T Int -> m Pattern
sampleHammingRange pat dist = do
  r          <- runT dist
  (sample:_) <- sampleHammingDistance pat r 1
  return sample


-- Samples patterns of hamming distance r of the given pattern
sampleHammingDistance :: MonadRandom m => Pattern -> Int -> Int -> m [Pattern]
sampleHammingDistance pat r numSamples
  = liftM (map (V.fromList . multByPat)) coeffSamples
      where
        n                = V.length pat
        basePerm         = toArray $ replicate r (-1) ++ replicate (n-r) 1
        coeffSamples     = replicateM numSamples $ shuffle basePerm
        multByPat coeffs = zipWith (*) coeffs (V.toList pat)


-- Percentage of sampled patterns in the ring of 'pat' which converge to 'pat'
-- pre: pattern of same size as network

-- A pattern ring of radius 'r' around 'pat' is the set of states with hamming
-- distance 'r' from 'pat'.
samplePatternRing :: MonadRandom m => HopfieldData -> Pattern -> Int -> m Double
samplePatternRing hs pat r = do
  samples           <- sampleHammingDistance pat r 100
  convergedPatterns <- mapM (repeatedUpdate $ weights hs) samples
  let numConverging =  length $ filter (==pat) convergedPatterns

  return $ numConverging ./. (length samples)


-- TODO include ring 0? This is 1 iff pattern is a fixed point.
--
-- Percentage convergence for each ring of 'pat' (excluding the trivial ring 0)
-- pre: pattern of same size as network
samplePatternBasin :: (MonadRandom m) => BasinMeasure m [Double]
samplePatternBasin hs pat = mapM (samplePatternRing hs pat)  [1..n]
  where
    n = V.length pat


-- Measures pattern's basin of attraction using the Storkey-Valabregue method
-- pre: pattern of same size as network
measurePatternBasin :: (MonadRandom m) => BasinMeasure m Int
measurePatternBasin hs pat = do
  t_mus <- samplePatternBasin hs pat
  return $ fromMaybe n $ findIndex (<0.9) t_mus
    where
      n   = V.length pat


-- -----------------------------------------------------------------------------
-- Functions relating to measuring errors in a network

compTerm :: HopfieldData -> Int -> Int -> Int
compTerm hs index n = - (pat V.! n) * (computeH (weights hs) pat n - pat V.! n)
                        where pat = (patterns hs) !! index


checkFixed :: HopfieldData -> Int -> Bool
checkFixed hs index = all (\x -> compTerm hs index x <= 1) [0.. V.length ((patterns hs) !! index) - 1]


-- | @measureError hopfield@: Measures the percentage of patterns in the network
-- which are NOT fixed points. That is, it measures the *actual* error
measureError :: HopfieldData -> Double
measureError hs = num_errors ./. num_pats
  where
    fixed_points = map (checkFixed hs) [0..num_pats-1]
    num_errors   = length $ filter not fixed_points
    num_pats     = length $ patterns hs
