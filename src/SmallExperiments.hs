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
import Utils (Type(H), patternGen)

import Clusters

main :: IO ()
main = do

  g <- getStdGenx

  putStrLn "basinsGivenStdT2 Hebbian 50 4 25 5"
  print $ evalRand (basinsGivenStdT2 Hebbian 50 4 25 5) g

  putStrLn "basinsGivenStdT2 Storkey 50 4 25 5"
  print $ evalRand (basinsGivenStdT2 Storkey 50 4 25 5) g

  putStrLn "basinsGivenStdT2 Hebbian 50 4 25 10"
  print $ evalRand (basinsGivenStdT2 Hebbian 50 4 25 10) g

  putStrLn "basinsGivenStdT2 Storkey 50 4 25 10"
  print $ evalRand (basinsGivenStdT2 Storkey 50 4 25 10) g
