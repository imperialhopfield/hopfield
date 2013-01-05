{-# LANGUAGE ParallelListComp #-}

module Main where

import Control.Monad (replicateM, liftM)

import Hopfield.Hopfield
import Hopfield.Clusters
import Hopfield.Util


main :: IO ()
main = do

  putStrLn "basinsGivenStdT2 Hebbian 50 6 25 5"
  avg1 <- liftM average $ replicateM 10 (basinsGivenStdT2 Hebbian 50 6 25 5)
  print avg1

  putStrLn "basinsGivenStdT2 Storkey 50 6 25 5"
  avg2 <- liftM average $ replicateM 10 (basinsGivenStdT2 Storkey 50 6 25 5)
  print avg2

  putStrLn "basinsGivenStdT2 Hebbian 50 4 25 10"
  avg3 <- liftM average $ replicateM 10 (basinsGivenStdT2 Hebbian 50 4 25 10)
  print avg3

  putStrLn "basinsGivenStdT2 Storkey 50 4 25 10"
  avg4 <- liftM average $ replicateM 10 (basinsGivenStdT2 Storkey 50 4 25 10)
  print avg4
