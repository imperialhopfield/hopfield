{-# LANGUAGE ParallelListComp #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.Random
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

  putStrLn "T2 experiment with 1 cluster with no average but lists"
  let avgs =  replicate 10 $ experimentUsingT2NoAvg Hebbian 100 10
  printMList avgs (replicate 10 prettyList)
