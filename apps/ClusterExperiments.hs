{-# LANGUAGE ParallelListComp #-}

module Main where


import Control.Monad (replicateM)
import Control.Monad.Random
import Control.Parallel.Strategies
import System.Environment

import Hopfield.Analysis
import Hopfield.Clusters
import Hopfield.Hopfield
import Hopfield.Measurement
import Hopfield.SuperAttractors
import Hopfield.Util


oneIteration :: Int -> Int -> Double -> Int-> [(Double, Double)]
oneIteration networkSize clusterSize dev_step i = zip cs deviations
      where
        f x = evalRand (basinsGivenStdT2 Hebbian networkSize clusterSize mean x) (mkStdGen i)
        unevaluated = map f deviations
        cs = unevaluated `using` parList rdeepseq
        mean = networkSize ./ 2.0
        deviations = [0.0, dev_step .. networkSize ./ 8.0]


performAndPrintT2 :: Int -> Int -> Int -> Double -> IO ()
performAndPrintT2 neurons clusterSize iterations dev_step = do
    putStrLn $ "T2 neurons  " ++ show neurons ++ "  cluster " ++ show clusterSize
    mapM_ print $ map (oneIteration neurons clusterSize dev_step) [0.. iterations]


main :: IO ()
main = do

  args <- getArgs

  case args of
    ("big": _)    -> performAndPrintT2 100 10 10 3.0
    ("simple": _) -> performAndPrintT2 60  6  10 1.0
    otherwise     -> putStrLn "Invalid argument"

