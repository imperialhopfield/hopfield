{-# LANGUAGE ParallelListComp #-}

module Main where


import Control.Monad (replicateM)
import Control.Monad.Random
import Control.Parallel.Strategies

import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)

import Hopfield.Analysis
import Hopfield.Clusters
import Hopfield.Hopfield
import Hopfield.Measurement
import Hopfield.SuperAttractors
import Hopfield.Util
import Hopfield.TestUtil (Type(H), patternGen)



main :: IO ()
main = do

  -- Commented for efficiency reasons
  -- putStrLn "T2 experiment with 1 cluster with no average but lists"
  -- let avgs =  replicate 10 $ experimentUsingT2NoAvg Hebbian 100 10
  -- printMList avgs (replicate 10 prettyList)


  g <- getStdGen

  let networkSize = 100
      clusterSize = 10
      mean = networkSize ./. (2 :: Int)
      deviations = [0.0, 2.0, networkSize ./. (8 :: Int)]
      f x = evalRand (basinsGivenStdT2 Hebbian networkSize clusterSize mean x) g
      unevaluated = map f deviations
      cs = unevaluated `using` parList rdeepseq
  print cs
