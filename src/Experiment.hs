{-# LANGUAGE ParallelListComp #-}

module Main where

import Analysis
import Control.Monad (replicateM)
import Data.List
import Hopfield
import Measurement
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import SuperAttractors
import System.Random
import Utils (Type(H), patternGen)



genIO :: Gen a -> IO a
genIO g = do
    rndInt <- randomIO
    stdGen <- getStdGen
    return $ unGen g stdGen rndInt


attachLabels :: (Show a, Show b) => [a] -> [b] -> [Char]
attachLabels labels items
  = concat [ concat [show l, ": ", show i, "\n"] | l <- labels | i <- items ]


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
    putStrLn $ attachLabels degrees expErrs


    -- Original pattern as the sole pattern in the network
    putStrLn "Building networks..."
    let nets = buildNetworks pats degrees patCombiner


    --Check if pattern is fixed.
    let patErrs = [ n | n <- nets, checkFixed n 0]
    if not $ null patErrs
        then putStrLn $
        -- TODO FIX TO RETURN DEGREES NOT NETWORKS
            "WARNING: The following degrees have produced networks where the pattern is NOT a fixed point:\n" ++
              show patErrs ++ "\n"
        else putStrLn "Pattern is always a fixed point\n"


    putStrLn "Measuring basins of attraction"
    results <- measureMultiBasins measurePatternBasin nets originPat

    putStrLn $ attachLabels degrees results
