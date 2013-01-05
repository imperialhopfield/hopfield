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

oneIterationT2 :: Int -> Int -> Double -> Int-> [(Double, Double)]
oneIterationT2 networkSize clusterSize dev_step i = zip cs deviations
      where
        f x = evalRand (basinsGivenStdT2 Hebbian networkSize clusterSize mean x) (mkStdGen i)
        unevaluated = map f deviations
        cs = unevaluated `using` parList rdeepseq
        mean = networkSize ./ 2.0
        deviations = [0.0, dev_step .. networkSize ./ 8.0]


oneIterationT1 :: Int -> Int -> Double -> Int-> [(Double, Double)]
oneIterationT1 networkSize clusterSize p_step i = zip cs probabilities
      where
        f x = evalRand (basinsGivenProbabilityT1 Hebbian networkSize clusterSize x) (mkStdGen i)
        unevaluated = map f probabilities
        cs = unevaluated `using` parList rdeepseq
        probabilities = [0.0, p_step .. 0.6]


performAndPrint :: ExpType -> Int -> Int -> Int -> Double -> IO ()
performAndPrint expType neurons clusterSize iterations step = do
    putStrLn $ "T2 neurons  " ++ show neurons ++ "  cluster " ++ show clusterSize
    mapM_ print $ map (iterationFunc neurons clusterSize step) [0.. iterations]
    where iterationFunc = case expType of
                          T1 -> oneIterationT1
                          T2 -> oneIterationT2


main :: IO ()
main = do

  args <- getArgs

  case args of
    ("big":t: _)   -> performAndPrint (read t) 100 10 10 3.0
    ("small":t: _) -> performAndPrint (read t) 60  6  10 1.0
    otherwise     -> putStrLn "Invalid argument"

