{-# LANGUAGE ParallelListComp #-}

module Hopfield.ClusterExperiments where
-- Cluster Experiments which were performed by Federico

import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies

import Hopfield.Clusters
import Hopfield.Hopfield
import Hopfield.Util


-- Data type which gives the type of the experiment
-- T1: bit flipping
-- T2: Gaussian distributed Hamming distance
data ExpType = T1 | T2
        deriving (Eq, Show, Read)


-- Runs one iteration  of an experiment with 1 cluster
oneIteration1 :: ExpType -> LearningType -> Int -> Int -> Double -> Double -> Double -> Int-> [(Double, Double)]
oneIteration1 expType learnType networkSize clusterSize start stop p_step i
  = zip cs values
  where
    f x = evalRand (evaluatedFunction x) (mkStdGen i)
    unevaluated = map f values
    cs = unevaluated `using` parList rdeepseq
    values = [start, p_step .. stop]
    evaluatedFunction = case expType of
      T1 -> basinsGivenProbabilityT1 learnType networkSize clusterSize
      T2 -> basinsGivenStdT2 learnType networkSize clusterSize (networkSize ./ 2.0)


-- Runs multiple iterations of an experiment with one cluster
-- Prints information to the user about the parameters of the experiment
performAndPrint1 :: ExpType -> LearningType -> Int -> Int -> Double -> Double -> Double -> Int -> IO ()
performAndPrint1 expType learnType neurons clusterSize start stop step iterations = do
  putStrLn $ "Experiment type" ++ show expType
  putStrLn $ "Only one clusters"
  putStrLn $ "neurons  " ++ show neurons ++ "  cluster " ++ show clusterSize
  putStrLn $ "performed for " ++ show iterations ++ " iterations"
  mapM_ print $ map (oneIteration1 expType learnType neurons clusterSize start stop step) [0.. iterations]


-- Runs one iteration  of an experiment with 2 clusters
oneIteration2 :: ExpType -> LearningType -> Int -> Int -> Double -> Double -> Double -> Double -> Int-> [(Double, (Double, Double))]
oneIteration2 expType learnType networkSize clusterSize val1 start2 stop2 p_step2 i
  = zip values cs
  where
    f x = evalRand (evaluatedFunction x) (mkStdGen i)
    unevaluated = map f values
    cs = unevaluated `using` parList rdeepseq
    values = [start2, start2 + p_step2 .. stop2]
    evaluatedFunction = case expType of
      T1 -> basinsGivenProbabilityT1With2Clusters learnType networkSize clusterSize val1
      T2 -> basinsGivenStdT2With2Clusters learnType networkSize clusterSize (networkSize ./ 2.0) (networkSize ./ 2.0) val1


-- Runs multiple iterations of an experiment with 2 clusters
-- Prints information to the user about the parameters of the experiment
performAndPrint2 :: ExpType -> LearningType -> Int -> Int -> Double -> Double -> Double -> Double -> Int -> IO ()
performAndPrint2 expType learnType neurons clusterSize val1 start2 stop2 step2 iterations = do
  putStrLn $ "Experiment type " ++ show expType
  putStrLn $ "Two clusters"
  putStrLn $ "neurons  " ++ show neurons ++ "  cluster " ++ show clusterSize
  putStrLn $ "performed for " ++ show iterations ++ " iterations"
  putStrLn $ "fixing the parameter(prob for T1 or std dev for T2) for the first cluster to be " ++ show val1
  putStrLn $ "varying the parameter for the second cluster between" ++ show start2 ++ "and " ++ show stop2
  seeds <- replicateM iterations $ getRandomR (0 :: Int, 1000 :: Int)
  mapM_ print $ map (oneIteration2 expType learnType neurons clusterSize val1 start2 stop2 step2) seeds


-- Called from the main of apps/ExperimentsMain.hs when the first argument of
-- the executable is 'cluster'
run :: [String] -> IO ()
run args = do

  case args of
    ("1": t : l: n : c : start : stop : step: iterations: _)-> performAndPrint1 (read t) (read l) (read n) (read c) (read start) (read stop) (read step) (read iterations)
    ("2": t : l: n : c : fixed : start : stop : step: iterations: _)-> performAndPrint2 (read t) (read l) (read n) (read c) (read fixed) (read start) (read stop) (read step) (read iterations)
    _ -> error "invalid arguments"
