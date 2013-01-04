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



genIO :: Gen a -> IO a
genIO g = do
    rndInt <- randomIO
    stdGen <- getStdGen
    return $ unGen g stdGen rndInt


errorHeader :: String
errorHeader = "Degree\tExpected error\n"


basinHeader :: String
basinHeader = "Degree\tBasin size\n"




main :: IO ()
main = do

    let n          = 50
        numRandoms = 8
        maxDegree  = 8


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


    putStrLn $ "Expected network errors: "
    let expErrs = [ computeErrorSuperAttractorNumbers d p n | d <- degrees ]
    putStrLn $ attachLabels errorHeader degrees expErrs


    putStrLn $ "Hamming distance between origin pattern and random patterns:"
    let hammingDists  = map (hammingDistance originPat) randomPats
        hammingPct    = map (./. n) hammingDists :: [Double]
    putStrLn $ prettyList hammingDists
    putStrLn $ toPercents hammingPct ++ "\n"

    -- Original pattern as the sole pattern in the network
    putStrLn "Building networks..."
    let nets = buildNetworks pats degrees patCombiner


    --Check if pattern is fixed.
    let patErrs = [ d | d <- degrees | net <- nets , not $ checkFixed net originIndex]
    if not $ null patErrs
        then putStrLn $
            "WARNING: The following degrees have produced networks where the pattern is NOT a fixed point:\n" ++
              prettyList patErrs ++ "\n"
        else putStrLn "Pattern is always a fixed point\n"


    putStrLn "Measuring basins of attraction"
    results <- measureMultiBasins measurePatternBasin nets originPat

    putStrLn $ attachLabels basinHeader degrees results

    -- putStrLn "T1 experiment with 1 cluster"
    -- putStrLn $ show $ evalRand (repeatExperiment experimentUsingT1 Storkey 1 50 8) (mkStdGen 1)


    putStrLn "T1 experiment with 1 cluster with no average but lists"
    print $ evalRand (replicateM 10 $ experimentUsingT1NoAvg Hebbian 100 10) (mkStdGen 1)

