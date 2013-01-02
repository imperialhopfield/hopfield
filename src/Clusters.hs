{-# LANGUAGE PatternGuards #-}

module Clusters where


-- Module which deals with pattern cluster generation and realted functions.
-- Implements probabilistic rewiring using Hamming distance.

import qualified Data.Vector as V
import           Control.Monad.Random (MonadRandom)
import           Control.Monad (liftM, replicateM)

import Common
import Hopfield
import Measurement
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



-- Caller has to take care with setting the mean and stdDev such that
-- the sampled numbers tend to be in the interval [0 .. size -1]
-- Implements the T2 method descirbed by Federico
-- Sample a Gaussia distribution with given mean and std dev
-- Round sampled numbers to integers
-- Use the integers to generate patters of the form 1 1 1... 1 -1 -1 -1
-- which will have their Hamming distance normally distributed
getGaussianCluster :: MonadRandom  m => Method -> Pattern -> Double -> Double -> Int -> m [Pattern]
getGaussianCluster method originPat mean stdDev size
  | mean > fromIntegral patSize = error "the mean cannot be greater than the size of the pattern in getGaussianCluster"
  | otherwise = do
      normal_values   <- replicateM size (normal mean stdDev)
      return $ map encoding $ map round normal_values
        where encoding x = V.fromList [ valueAtIndex y x | y <- [0 .. patSize - 1]]
              patSize = V.length originPat
              valueAtIndex y x = if (y <=x) then 1 else (smallerValue method)
              smallerValue x = case x of
                                Hopfield -> -1
                                _        -> 0


-- TODO pass in LearningType parameter to compare Hebb with S

getBasinsGivenProbabilityT1 :: MonadRandom m => Int -> Int -> Double -> m Double
getBasinsGivenProbabilityT1 networkSize clusterSize p
  =  do
     originPat <- randomBinaryVector networkSize
     cluster   <- getCluster Hopfield originPat p clusterSize
     let hopfield = buildHopfieldData Hebbian cluster
     basinSizes <- mapM (measurePatternBasin hopfield) cluster
     return $ average basinSizes

experimentUsingT1 :: MonadRandom m => Int -> Int -> m Double
experimentUsingT1 networkSize clusterSize
  = do
    basinAvgs <- mapM (getBasinsGivenProbabilityT1 networkSize clusterSize) [0.0, 0.01 .. 0.5]
    return $ average basinAvgs


repeatExperimentT1 :: MonadRandom m => Int -> Int -> Int -> m Double
repeatExperimentT1 nrExperiments networkSize clusterSize
  = liftM average $ replicateM nrExperiments (experimentUsingT1 networkSize clusterSize)
