{-# LANGUAGE ParallelListComp #-}

module Main where

import Analysis
import Control.Monad (replicateM)
import Control.Monad.Random
import Hopfield
import Measurement
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import SuperAttractors
import Util
import Utils (Type(H), patternGen)

import Clusters

main :: IO ()
main = do

  putStrLn "T2 experiment with 1 cluster with no average but lists"
  let avgs =  replicate 10 $ experimentUsingT2NoAvg Hebbian 100 10
  printMList avgs (replicate 10 prettyList)
