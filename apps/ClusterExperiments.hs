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
import Hopfield.TestUtil (Type(H), patternGen)


oneIteration :: Int -> Int -> Int -> [(Double, Double)]
oneIteration networkSize clusterSize i = zip cs deviations
      where
        f x = evalRand (basinsGivenStdT2 Hebbian networkSize clusterSize mean x) (mkStdGen i)
        unevaluated = map f deviations
        cs = unevaluated `using` parList rdeepseq
        mean = networkSize ./ 2.0
        deviations = [0.0, 1.0 .. networkSize ./ 8.0]


performAndPrintT2 :: Int -> Int -> Int -> IO ()
performAndPrintT2 neurons clusterSize iterations = do
    putStrLn $ "T2 neurons" ++ show neurons ++ "cluster " ++ show clusterSize
    mapM_ print $ map (oneIteration neurons clusterSize) [0.. iterations]


main :: IO ()
main = do

  args <- getArgs

  case (head args) of
    "big"    -> performAndPrintT2 100 10 10
    "simple" -> performAndPrintT2 100 10 10

