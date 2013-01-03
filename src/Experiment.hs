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



genIO :: Gen a -> IO a
genIO g = do
    rndInt <- randomIO
    stdGen <- getStdGen
    return $ unGen g stdGen rndInt


attachLabels :: (Show a, Show b) => [Char] -> [a] -> [b] -> [Char]
attachLabels header labels items
  = header ++ concat list
  where
    list  = [ concat [show l, "\t", show i, "\n"] | l <- labels | i <- items ]


errorHeader :: [Char]
errorHeader = "Degree\tExpected error\n"


basinHeader :: [Char]
basinHeader = "Degree\tBasin size\n"




main :: IO ()
main = do

    let n          = 50
        numRandoms = 8
        maxDegree  = 8


    -- The super attractor - primary care giver
    originPat <- genIO $ patternGen H n

    -- Sample random patterns - noise data
    randomPats <- replicateM numRandoms $ genIO $ patternGen H n


    let pats        = originPat:randomPats
        originIndex = 0                         -- index of main pattern
        degrees     = powersOfTwo maxDegree
        patCombiner = oneSuperAttr


    putStrLn $ "Expected network errors: "
    let expErrs = [ computeErrorSuperAttractorNumbers d (length pats) n | d <- degrees ]
    putStrLn $ attachLabels errorHeader degrees expErrs


    -- Original pattern as the sole pattern in the network
    putStrLn "Building networks..."
    let nets = buildNetworks pats degrees patCombiner


    --Check if pattern is fixed.
    let patErrs = [ d | d <- degrees | net <- nets , not $ checkFixed net originIndex]
    if not $ null patErrs
        then putStrLn $
            "WARNING: The following degrees have produced networks where the pattern is NOT a fixed point:\n" ++
              show patErrs ++ "\n"
        else putStrLn "Pattern is always a fixed point\n"


    putStrLn "Measuring basins of attraction"
    results <- measureMultiBasins measurePatternBasin nets originPat

    putStrLn $ attachLabels basinHeader degrees results

    putStrLn "T1 experiment with 1 cluster"
    putStrLn $ show $ evalRand (repeatExperiment experimentUsingT1 Hebbian 20 100 10) (mkStdGen 1)
