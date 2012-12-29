{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions to measure a network's basins of attractions
module Basin (
    BasinMeasure
  , samplePatternBasin
  , measurePatternBasin
) where

import           Control.Monad.Random (MonadRandom)
import           Control.Monad.Random.Class (getRandom)
import           Data.List
import           Data.Maybe
import qualified Data.Vector as V
import           Data.Random.Extras (sample)
import           Data.RVar (runRVar)
import           Data.Word (Word32)
import           Hopfield
import           Util ((./.))


-- A function computing some measure of a pattern's basin in the given network
type BasinMeasure m a = HopfieldData -> Pattern -> m a


-- Generate list of states with hamming distance r of the given pattern
withHammingDistance :: Pattern -> Int -> [Pattern]
withHammingDistance pat r = map (V.fromList . multByPat) coeffsList
  where
    n                = V.length pat
    perms            = sequence $ replicate n [1, -1]
    hasDistanceR xs  = replicate r (-1) == filter (== (-1)) xs
    coeffsList       = filter hasDistanceR perms
    multByPat coeffs = zipWith (*) coeffs (V.toList pat)


-- Percentage of sampled patterns in the ring of 'pat' which converge to 'pat'
-- pre: pattern of same size as network

-- A pattern ring of radius 'r' around 'pat' is the set of states with hamming
-- distance 'r' from 'pat'.
samplePatternRing :: forall m . MonadRandom m => HopfieldData -> Pattern -> Int -> m Double
samplePatternRing hs pat r = do
  let rSamples      =  sample 100 $ withHammingDistance pat r
  samples           <- runRVar rSamples (getRandom :: m Word32)
  convergedPatterns <- mapM (repeatedUpdate $ weights hs) samples
  let numConverging =  length $ filter (==pat) convergedPatterns

  return $ numConverging ./. (length samples)


-- TOOD include ring 0? This is 1 iff pattern is a fixed point.
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
