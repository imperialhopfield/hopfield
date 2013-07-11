{-# LANGUAGE ParallelListComp #-}

module Hopfield.Experiments.SmallExperiments where

-- Module use to perform small experiments that prove that Storkey learning
-- has a bigger basin size than Hebbian learning

import Control.Applicative
import Control.Monad

import Hopfield.Clusters
import Hopfield.Hopfield
import Hopfield.Util

_REPETITIONS :: Int
_REPETITIONS = 10

-- Experiments are performed using the T2 method
-- (learning type, number of neurons, cluster size, mean of cluster, std dev)
runs :: [(LearningType, Int, Int, Double, Double)]
runs = [ (Hebbian, 50, 6, 25, 5)
       , (Storkey, 50, 6, 25, 5)
       , (Hebbian, 50, 4, 25, 10)
       , (Storkey, 50, 4, 25, 10)
       ]

main :: IO ()
main = do
 forM_ runs $ \(method, a, b, c, d) ->
  print =<< ((average <$> replicateM _REPETITIONS (basinsGivenStdT2 method a b c d)) :: IO Double)
