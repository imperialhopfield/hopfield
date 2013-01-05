{-# LANGUAGE ParallelListComp #-}

module Main where


import Control.Monad (replicateM)
import Control.Monad.Random
import Control.Parallel.Strategies

import Hopfield.Analysis
import Hopfield.Clusters
import Hopfield.Hopfield
import Hopfield.Measurement
import Hopfield.SuperAttractors
import Hopfield.Util
import Hopfield.TestUtil (Type(H), patternGen)


oneIteration :: Int -> Int -> Int -> [(Double, Double)]
oneIteration networkSize clusterSize i = zip cs deviations
      where
        f x = evalRand (basinsGivenStdT2 Hebbian networkSize clusterSize mean x) (mkStdGen i)
        unevaluated = map f deviations
        cs = unevaluated `using` parList rdeepseq
        mean = networkSize ./ 2.0
        deviations = [0.0, 1.0 .. networkSize ./ 8.0]


main :: IO ()
main = do

  -- Commented for efficiency reasons
  -- let avgs =  replicate 10 $ experimentUsingT2NoAvg Hebbian 100 10
  -- printMList avgs (replicate 10 prettyList)

  putStrLn "T2 in IO() to be able to use parallel map with 50 neurons cluster of size 5"
  mapM_ print $ map (oneIteration 60 6) [0.. 5]

