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
  -- let avgs =  replicate 10 $ experimentUsingT2NoAvg Hebbian 100 10
  -- printMList avgs (replicate 10 prettyList)


  putStrLn "T2 in IO() to be able to use parallel map with 50 neurons cluster of size 5"

  g <- getStdGen

  let networkSize = 50
      clusterSize = 5
      mean = networkSize ./. (2 :: Int)
      deviations = [0.0, 2.0 .. networkSize ./. (8 :: Int)]
      f x = evalRand (basinsGivenStdT2 Hebbian networkSize clusterSize mean x) g
      unevaluated = map f deviations
      cs = unevaluated `using` parList rdeepseq
  print cs
