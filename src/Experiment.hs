{-# LANGUAGE ParallelListComp #-}

module Main where

import Analysis
import Common
import Control.Monad (replicateM)
import Control.Monad.Random
import ExpUtil
import Hopfield
import Measurement
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import SuperAttractors
import Util
import Utils (Type(H), patternGen)

import Clusters



genIO :: Gen a -> IO a
genIO g = do
    rndInt <- randomIO
    stdGen <- getStdGen
    return $ unGen g stdGen rndInt


errorHeader :: String
errorHeader = "Degree\tExpected error"


basinHeader :: String
basinHeader = "Degree\tBasin size"




main :: IO ()
main = do

    let n          = 100    -- number of neurons
        numRandoms = 8      -- number of random patterns to include
        maxDegree  = 32     -- maximum degree of super attractor


    -- The super attractor - primary care giver
    originPat <- genIO $ patternGen H n

    -- Sample random patterns with Hamming distance between 25-75% from origin
    -- This is to ensure that this is a pure super attractor experiment
    -- and not a cluster one!
    let minHamming = round $ n .* (0.25 :: Double)
        maxHamming = round $ n .* (0.75 :: Double)
        dist       = hammingDistribution n (minHamming, maxHamming)

    randomPats <- replicateM numRandoms $ sampleHammingRange originPat dist


    let pats        = originPat:randomPats
        p           = length pats
        originIndex = 0                         -- index of main pattern
        degrees     = powersOfTwo maxDegree
        patCombiner = oneSuperAttr


    putStrLn $ unwords [show n, "neurons.", "Super attractor plus", show numRandoms, "random patterns.\n"]


    -- Compute probability of error
    doErrorProb n p degrees


    -- Compute hamming distance
    doHamming originPat randomPats "origin" "random"


    putStrLn "Building networks..."
    let nets = buildNetworks pats degrees patCombiner


    --Check if pattern is fixed.
    doCheckFixed (zip degrees nets) originIndex "degrees"


    putStrLn "Measuring basins of attraction"
    let results = measureMultiBasins measurePatternBasin nets originPat

    putStrLn basinHeader
    printMList results [ \r -> attachLabel [pack d, pack r] | d <- degrees ]


    -- putStrLn "T1 experiment with 1 cluster"
    -- putStrLn $ show $ evalRand (repeatExperiment experimentUsingT1 Storkey 1 50 8) (mkStdGen 1)


    putStrLn "T1 experiment with 1 cluster with no average but lists"
    print $ evalRand (replicateM 10 $ experimentUsingT1NoAvg Hebbian 100 10) (mkStdGen 1)

