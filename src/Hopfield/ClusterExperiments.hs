{-# LANGUAGE ParallelListComp #-}

module Hopfield.ClusterExperiments where

import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies

import Hopfield.Clusters
import Hopfield.Hopfield
import Hopfield.Util


data ExpType = T1 | T2
        deriving (Eq, Show, Read)


oneIteration1 :: ExpType -> Int -> Int -> Double -> Double -> Double -> Int-> [(Double, Double)]
oneIteration1 expType networkSize clusterSize start stop p_step i = zip cs values
  where
    f x = evalRand (evaluatedFunction x) (mkStdGen ( i + 100))
    unevaluated = map f values
    cs = unevaluated `using` parList rdeepseq
    values = [start, p_step .. stop]
    evaluatedFunction = case expType of
      T1 -> basinsGivenProbabilityT1 Hebbian networkSize clusterSize
      T2 -> basinsGivenStdT2 Hebbian networkSize clusterSize (networkSize ./ 2.0)


performAndPrint1 :: ExpType -> Int -> Int -> Double -> Double -> Double -> Int -> IO ()
performAndPrint1 expType neurons clusterSize start stop step iterations = do
  putStrLn $ "Experiment type" ++ show expType
  putStrLn $ "Only one clusters"
  putStrLn $ "neurons  " ++ show neurons ++ "  cluster " ++ show clusterSize
  putStrLn $ "performed for " ++ show iterations ++ " iterations"
  mapM_ print $ map (oneIteration1 expType neurons clusterSize start stop step) [0.. iterations]


oneIteration2 :: ExpType -> Int -> Int -> Double -> Double -> Double -> Double -> Int-> [(Double, Double)]
oneIteration2 expType networkSize clusterSize val1 start2 stop2 p_step2 i = zip cs values
  where
    f x = evalRand (evaluatedFunction x) (mkStdGen (i + 100))
    unevaluated = map f values
    cs = unevaluated `using` parList rdeepseq
    values = [start2, start2 + p_step2 .. stop2]
    evaluatedFunction = case expType of
      T1 -> basinsGivenProbabilityT1With2Clusters Hebbian networkSize clusterSize val1
      T2 -> basinsGivenStdT2With2Clusters Hebbian networkSize clusterSize (networkSize ./ 2.0) (networkSize ./ 2.0) val1


performAndPrint2 :: ExpType -> Int -> Int -> Double -> Double -> Double -> Double -> Int -> IO ()
performAndPrint2 expType neurons clusterSize val1 start2 stop2 step2 iterations = do
  putStrLn $ "Experiment type " ++ show expType
  putStrLn $ "Two clusters"
  putStrLn $ "neurons  " ++ show neurons ++ "  cluster " ++ show clusterSize
  putStrLn $ "performed for " ++ show iterations ++ " iterations"
  putStrLn $ "fixing the parameter(prob for T1 or std dev for T2) for the first cluster to be " ++ show val1
  putStrLn $ "varying the parameter for the second cluster between" ++ show start2 ++ "and " ++ show stop2
  seeds <- replicateM iterations $ getRandomR (20 :: Int, 1000 :: Int)
  mapM_ print $ map (oneIteration2 expType neurons clusterSize val1 start2 stop2 step2) seeds



run :: [String] -> IO ()
run args = do

  case args of
    ("1": t : n : c : start : stop : step: iterations: _)-> performAndPrint1 (read t) (read n) (read c) (read start) (read stop) (read step) (read iterations)
    ("2": t : n : c : fixed : start : stop : step: iterations: _)-> performAndPrint2 (read t) (read n) (read c) (read fixed) (read start) (read stop) (read step) (read iterations)
    _ -> error "invalid arguments"
