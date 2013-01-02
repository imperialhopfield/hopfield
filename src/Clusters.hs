{-# LANGUAGE PatternGuards #-}

module Clusters where


-- Module which deals with pattern cluster generation and realted functions.
-- Implements probabilistic rewiring using Hamming distance.

import qualified Data.Vector as V
import           Control.Monad.Random (MonadRandom)
import           Control.Monad (liftM, replicateM)

import Common
import Util


--  @getPatternInCluster pat p@ gets a pattern in a cluster given by @pat@
-- by flipping each bit in the pattern with probability p.
getPatternInCluster :: MonadRandom  m => Method -> Pattern -> Double -> m Pattern
getPatternInCluster method originPat p
  = liftM V.fromList $ mapM transformBit (V.toList originPat)
  where transformBit x = do
          flip_bit <- gibbsSampling p
          let bit = if (odd flip_bit) then (flipBit method x) else x
          return bit


--  @getPatternInCluster pat p@ gets a pattern in a cluster given by @pat@
-- by flipping each bit in the pattern with probability p.
getCluster :: MonadRandom  m => Method -> Pattern -> Double -> Int -> m [Pattern]
getCluster method originPat p size
  = replicateM size (getPatternInCluster method originPat p)



-- TODO (Mihaela) test this
-- Caller has to take care with setting the mean and stdDev such that
-- the sampled numbers tend to be in the interval [0 .. size -1]
getGaussianCluster :: MonadRandom  m => Method -> Pattern -> Double -> Double -> Int -> m [Pattern]
getGaussianCluster method originPat mean stdDev size = do
   normal_values   <- replicateM size (normal mean stdDev)
   return $ map encoding $ map round normal_values
     where encoding x = V.fromList [ valueAtIndex y x | y <- [0 .. patSize - 1]]
           patSize = V.length originPat
           valueAtIndex y x = if (y <=x) then 1 else (smallerValue method)
           smallerValue x = case x of
                                Hopfield -> -1
                                _        -> 0
