{-# LANGUAGE PatternGuards #-}

module Clusters where


-- Module which deals with pattern cluster generation and realted functions.
-- Implements probabilistic rewiring using Hamming distance.

import qualified Data.Vector as V
import           Control.Monad.Random (MonadRandom)
import           Control.Monad (liftM, replicateM)

import Common
import Util



-- we have to look at
-- we use Gibbs sampling where 1 represents flip bit and 0 represents not flip bit

--  @getPatternInCluster pat p@ gets a pattern in a cluster given by @pat@
-- by flipping each bit in the pattern with probability p.
getPatternInCluster ::  MonadRandom  m => Pattern -> Double -> m Pattern
getPatternInCluster originPat p = liftM V.fromList $ mapM flipBit (V.toList originPat)
  where
    flipBit x = do
      flip_bit <- gibbsSampling p
      return $ if (flip_bit == 1) then 1 - x else x

--  @getPatternInCluster pat p@ gets a pattern in a cluster given by @pat@
-- by flipping each bit in the pattern with probability p.
getCluster :: MonadRandom  m => Pattern -> Double -> Int -> m [Pattern]
getCluster originPat p size = replicateM size (getPatternInCluster originPat p)


-- TODO (Mihaela) test this
-- Caller has to take care with setting the mean and stdDev such that
-- the sampled numbers tend to be in the interval [0 .. size -1]
getGaussianCluster :: MonadRandom  m => Pattern -> Double -> Double -> Int -> m [Pattern]
getGaussianCluster originPat mean stdDev size = do
   normal_values   <- replicateM size (normal mean stdDev)
   return $ map getPatternEncoding (map round normal_values)
     where getPatternEncoding x = V.fromList $ map (\y -> if (y <=x) then 1 else -1 ) [0 .. patSize - 1]
           patSize = V.length originPat
