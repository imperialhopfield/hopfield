{-# LANGUAGE ParallelListComp #-}

module Hopfield.ExperimentSuper2 where

import Control.Monad (replicateM)
import Control.Monad.Random
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)

import Hopfield.Common
import Hopfield.ExpUtil
import Hopfield.Hopfield (LearningType (..))
import Hopfield.Measurement
import Hopfield.SuperAttractors
import Hopfield.TestUtil (Type(H), patternGen)
import Hopfield.Util


genIO :: Gen a -> IO a
genIO g = do
    rndInt <- randomIO
    stdGen <- getStdGen
    return $ unGen g stdGen rndInt


basinHeader :: String
basinHeader = "Degree\tOrigin basin\tNew basin"


main :: IO ()
main = do

    let n          = 100    -- number of neurons
        numRandoms = 8      -- number of random patterns to include
        maxDegree  = 32     -- maximum degree of second super attractor
        fstDegree  = 8      -- (fixed) degree of first super attractor


    -- The first super attractor - primary care giver
    originPat <- genIO $ patternGen H n

    -- Sample random patterns with Hamming distance between 25-75% from origin
    -- This is to ensure that this is a pure super attractor experiment
    -- and not a cluster one!
    let minHamming = round $ n .* (0.25 :: Double)
        maxHamming = round $ n .* (0.75 :: Double)
        dist       = hammingDistribution n (minHamming, maxHamming)

    randomPats <- replicateM numRandoms $ sampleHammingRange originPat dist

    -- The second super attractor - retraining
    newPat <- sampleHammingRange originPat dist



    let pats        = originPat:newPat:randomPats
        originIndex = 0                         -- index of main pattern
        newIndex    = fstDegree + 1             -- index of new pattern
        degrees     = powersOfTwo maxDegree
        patCombiner = twoSuperAttrOneFixed fstDegree


    putStrLn $ unwords [show n, "neurons.", "Two Super attractors plus", show numRandoms, "random patterns.\n"]


    -- Check hamming distances
    doHamming originPat randomPats "origin" "random"
    doHamming newPat randomPats "new" "random"


    putStrLn "Building networks...\n"
    let nets = buildNetworks pats degrees Hebbian patCombiner


    --Check if patterns are fixed.
    putStrLn "Checking original pattern"
    doCheckFixed (zip degrees nets) originIndex "degrees"
    putStrLn "Checking new pattern"
    doCheckFixed (zip degrees nets) newIndex "degrees"


    putStrLn "Measuring basins of attraction of origin"
    let resultsOrigin = measureMultiBasins measurePatternBasin nets originPat
    let resultsNew    = measureMultiBasins measurePatternBasin nets newPat



    let results = zipWith (\a b -> sequence [a, b]) resultsOrigin resultsNew
        printResults d rs = attachLabel $ [pack d] ++ map pack rs

    putStrLn basinHeader
    printMList results [ printResults d | d <- degrees ]

