{-# LANGUAGE ParallelListComp #-}

module Hopfield.SmallExperiments where

import Control.Applicative
import Control.Monad

import Hopfield.Clusters
import Hopfield.Hopfield
import Hopfield.Util


_REPETITIONS = 10

runs = [ (Hebbian, 50, 6, 25, 5)
       , (Storkey, 50, 6, 25, 5)
       , (Hebbian, 50, 4, 25, 10)
       , (Storkey, 50, 4, 25, 10)
       ]

main :: IO ()
main = do
 forM_ runs $ \(method, a, b, c, d) ->
  print =<< average <$> replicateM _REPETITIONS (basinsGivenStdT2 method a b c d)

