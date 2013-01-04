{-# LANGUAGE ParallelListComp #-}

module Main where

import Control.Monad.Random
import Hopfield

import Clusters

main :: IO ()
main = do

  g <- getStdGen

  putStrLn "basinsGivenStdT2 Hebbian 50 4 25 5"
  print $ evalRand (basinsGivenStdT2 Hebbian 50 4 25 5) g

  putStrLn "basinsGivenStdT2 Storkey 50 4 25 5"
  print $ evalRand (basinsGivenStdT2 Storkey 50 4 25 5) g

  putStrLn "basinsGivenStdT2 Hebbian 50 4 25 10"
  print $ evalRand (basinsGivenStdT2 Hebbian 50 4 25 10) g

  putStrLn "basinsGivenStdT2 Storkey 50 4 25 10"
  print $ evalRand (basinsGivenStdT2 Storkey 50 4 25 10) g
