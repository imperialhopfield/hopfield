{-# LANGUAGE PatternGuards #-}

module Hopfield.Clusters where


-- Module which deals with pattern cluster generation and related functions.
-- Implements probabilistic rewiring using Hamming distance.

import qualified Data.Vector as V
import           Control.Monad.Random (MonadRandom)
import           Control.Monad (liftM, replicateM)

import Hopfield.Common
import Hopfield.Hopfield
import Hopfield.Measurement
import Hopfield.Util


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
-- Implements the T2 method described by Federico
-- Sample a Gaussian distribution with given mean and std dev
-- Round sampled numbers to integers
-- Use the integers to generate patters of the form 1 1 1... 1 -1 -1 -1
-- which will have their Hamming distance normally distributed
getGaussianCluster :: MonadRandom  m => Method -> Pattern -> Int -> Double -> Double -> m [Pattern]
getGaussianCluster method originPat size mean stdDev
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

-- | @basinsGivenProbabilityT1 learning networkSize clusterSize p@
-- Gets the average basin of attraction of a cluster of size @clusterSize@
-- constructed using the T1 method given the flip probability @p@.
-- A hopfield network is trained (the type of training (Hebbian or Storkey) is
-- given by @learning@).
basinsGivenProbabilityT1 :: MonadRandom m => LearningType -> Int -> Int -> Double -> m Double
basinsGivenProbabilityT1 learning networkSize clusterSize p
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
    basinAvgs <- mapM (basinsGivenProbabilityT1 learning networkSize clusterSize) [0.0, 0.1 .. 0.5]
    return $ average basinAvgs

experimentUsingT1NoAvg :: MonadRandom m => LearningType -> Int -> Int -> m [(Double, Double)]
experimentUsingT1NoAvg learning networkSize clusterSize
  = do
  results <- mapM (basinsGivenProbabilityT1 learning networkSize clusterSize) probabilities
  return $ zip probabilities results
  where probabilities = [0.0, 0.1 .. 0.5]


-------

basinsGivenProbabilityT1With2Clusters :: MonadRandom m => LearningType -> Int -> Int -> Double -> Double -> m Double
basinsGivenProbabilityT1With2Clusters learning networkSize clusterSize p2 p1  =  do
     originPat1 <- randomSignVector networkSize
     originPat2 <- randomSignVector networkSize
     cluster1   <- getCluster Hopfield originPat1 clusterSize p1
     cluster2   <- getCluster Hopfield originPat2 clusterSize p2
     avgBasinsGivenPats learning (cluster1 ++ cluster2)


experimentUsingT1With2Clusters :: MonadRandom m => LearningType -> Int -> Int -> m Double
experimentUsingT1With2Clusters learning networkSize clusterSize
  = do
    basinAvgs <- mapM (basinsGivenProbabilityT1With2Clusters learning networkSize clusterSize 0.45) [0.0, 0.01 .. 0.5]
    return $ average basinAvgs


-------   Experiments using Gaussian distributed patterns

basinsGivenStdT2 :: MonadRandom m => LearningType -> Int -> Int -> Double -> Double -> m Double
basinsGivenStdT2 learning networkSize clusterSize mean std
  =  do
     originPat <- randomSignVector networkSize
     cluster   <- getGaussianCluster Hopfield originPat clusterSize mean std
     avgBasinsGivenPats learning cluster


experimentUsingT2 :: MonadRandom m => LearningType -> Int -> Int -> m Double
experimentUsingT2 learning networkSize clusterSize
  = do
    let mean = networkSize ./. (2 :: Int)
        deviations = [0.0, 2.0, networkSize ./. (8 :: Int)]
    basinAvgs <- mapM (basinsGivenStdT2 learning networkSize clusterSize mean) deviations
    return $ average basinAvgs


basinsGivenProbabilityT2With2Clusters :: MonadRandom m => LearningType -> Int -> Int ->
                                            Double -> Double -> Double -> Double -> m Double
basinsGivenProbabilityT2With2Clusters learning networkSize clusterSize mean1 mean2 std1 std2  =  do
     originPat1 <- randomSignVector networkSize
     originPat2 <- randomSignVector networkSize
     cluster1   <- getGaussianCluster Hopfield originPat1 clusterSize mean1 std1
     cluster2   <- getGaussianCluster Hopfield originPat2 clusterSize mean2 std2
     avgBasinsGivenPats learning (cluster1 ++ cluster2)


experimentUsingT2With2Clusters :: MonadRandom m => LearningType -> Int -> Int -> Double -> Double -> m Double
experimentUsingT2With2Clusters learning networkSize clusterSize mean1 mean2 = do
  let deviation1 =  networkSize ./. (10 :: Int)
      deviations = [0.0, 2.0 .. 2.0 * deviation1]
  basinAvgs <- mapM (basinsGivenProbabilityT2With2Clusters learning networkSize clusterSize mean2 mean1 deviation1) deviations
  return $ average basinAvgs


type1T2 :: MonadRandom m => LearningType -> Int -> Int -> m Double
type1T2 learning networkSize clusterSize
  = experimentUsingT2With2Clusters learning networkSize clusterSize mean1 mean2
    where mean1 = networkSize ./. (4 :: Int)
          mean2 = (networkSize * 3)./. (4 :: Int)


type2T2 :: MonadRandom m => LearningType -> Int -> Int -> m Double
type2T2 learning networkSize clusterSize
  = experimentUsingT2With2Clusters learning networkSize clusterSize mean1 mean2
  where mean1 = (networkSize * 5) ./. (12 :: Int)
        mean2 = (networkSize * 7) ./. (12 :: Int)



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
