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
getCluster :: MonadRandom  m => Method -> Pattern -> Int -> Double -> m [Pattern]
getCluster method originPat size p
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

-- | @getBasinsGivenProbabilityT1 learning networkSize clusterSize p@
-- Gets the average basin of attraction of a cluster of size @clusterSize@
-- constructed using the T1 method given the flip probability @p@.
-- A hopfield network is trained (the type of training (Hebbian or Storkey) is
-- given by @learning@).
getBasinsGivenProbabilityT1 :: MonadRandom m => LearningType -> Int -> Int -> Double -> m Double
getBasinsGivenProbabilityT1 learning networkSize clusterSize p
  =  do
     originPat <- randomSignVector networkSize
     cluster   <- getCluster Hopfield originPat clusterSize p
     avgBasinsGivenPats learning cluster


-- | @experimentUsingT1 learning networkSize clusterSize@
-- Gets the average basin of attraction obtained by iterating trough various
-- probabilities for flipping the bit when obtaining the cluster.
experimentUsingT1 :: MonadRandom m => LearningType -> Int -> Int -> m Double
experimentUsingT1 learning networkSize clusterSize
  = do
    basinAvgs <- mapM (getBasinsGivenProbabilityT1 learning networkSize clusterSize) [0.0, 0.01 .. 0.5]
    return $ average basinAvgs

-------


getBasinsGivenProbabilityT1With2Clusters :: MonadRandom m => LearningType -> Int -> Int -> Double -> Double -> m Double
getBasinsGivenProbabilityT1With2Clusters learning networkSize clustersTotalSize p2 p1  =  do
     originPat1 <- randomSignVector networkSize
     originPat2 <- randomSignVector networkSize
     cluster1   <- getCluster Hopfield originPat1 (clustersTotalSize `div` 2) p1
     cluster2   <- getCluster Hopfield originPat2 (clustersTotalSize `div` 2) p2
     avgBasinsGivenPats learning (cluster1 ++ cluster2)


experimentUsingT1With2Clusters :: MonadRandom m => LearningType -> Int -> Int -> m Double
experimentUsingT1With2Clusters learning networkSize clusterSize
  = do
    basinAvgs <- mapM (getBasinsGivenProbabilityT1With2Clusters learning networkSize clusterSize 0.45) [0.0, 0.01 .. 0.5]
    return $ average basinAvgs


-------   Experiments using Gaussian distributed patterns

getBasinsGivenStdT2 :: MonadRandom m => LearningType -> Int -> Int -> Double -> Double -> m Double
getBasinsGivenStdT2 learning networkSize clusterSize mean std
  =  do
     originPat <- randomSignVector networkSize
     cluster   <- getGaussianCluster Hopfield originPat mean std clusterSize
     avgBasinsGivenPats learning cluster





--------------- General used functions, independent of method

avgBasinsGivenPats :: MonadRandom m => LearningType -> [Pattern] -> m Double
avgBasinsGivenPats learning pats = do
  basinSizes <- mapM (measurePatternBasin hopfield) pats
  return $ average basinSizes
    where hopfield = buildHopfieldData learning pats


-- Repeats an experiment for a single cluster, and averages the results obtained
-- in each of the experiments.
repeatExperiment :: MonadRandom m => (LearningType -> Int -> Int -> m Double) -> LearningType -> Int -> Int -> Int -> m Double
repeatExperiment experiment learning nrExperiments networkSize clusterSize
  = liftM average $ replicateM nrExperiments (experiment learning networkSize clusterSize)
