{-# LANGUAGE ParallelListComp #-}

module Main where


import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies
import System.Environment

import Hopfield.Clusters
import Hopfield.Hopfield
import Hopfield.Util


data ExpType = T1 | T2
        deriving (Eq, Show, Read)


oneIteration :: ExpType -> Int -> Int -> Double -> Double -> Double -> Int-> [(Double, Double)]
oneIteration expType networkSize clusterSize start stop p_step i = zip cs values
  where
    f x = evalRand (evaluatedFunction x) (mkStdGen i)
    unevaluated = map f values
    cs = unevaluated `using` parList rdeepseq
    values = [start, p_step .. stop]
    evaluatedFunction = case expType of
      T1 -> basinsGivenProbabilityT1 Hebbian networkSize clusterSize
      T2 -> basinsGivenStdT2 Hebbian networkSize clusterSize (networkSize ./ 2.0)


performAndPrint :: ExpType -> Int -> Int -> Double -> Double -> Double -> Int ->IO ()
performAndPrint expType neurons clusterSize start stop step iterations = do
  putStrLn $ "Experiment type" ++ show expType
  putStrLn $ "neurons  " ++ show neurons ++ "  cluster " ++ show clusterSize
  putStrLn $ "performed for " ++ show iterations ++ " iterations"
  mapM_ print $ map (oneIteration expType neurons clusterSize start stop step) [0.. iterations]


main :: IO ()
main = do

  args <- getArgs
  case args of
    (t : n : c : iterations : start : stop : step: _)-> performAndPrint (read t) (read n) (read c) (read start) (read stop) (read step) (read iterations)
    _ -> error "invalid arguments"
