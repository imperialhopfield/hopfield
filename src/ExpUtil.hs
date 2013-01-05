{-# LANGUAGE ParallelListComp #-}

module ExpUtil where

import           Analysis (computeErrorSuperAttractorNumbers)
import           Common
import           Hopfield
import           Measurement
import           SuperAttractors (Degree)
import           Util
import qualified Data.Vector as V


-- Measure hamming distance from p to each of ps
doHamming :: Pattern -> [Pattern] -> String -> String -> IO ()
doHamming p ps pName psName = do
    let msg = unwords ["Hamming distance between", pName, "pattern and",
                        psName, "patterns:"]
    putStrLn msg
    let n            = V.length p
        hammingDists = map (hammingDistance p) ps
        hammingPct   = map (./. n) hammingDists :: [Double]
    putStrLn $ prettyList hammingDists
    putStrLn $ toPercents hammingPct ++ "\n"


-- Check if pattern is a fixed fixed points
doCheckFixed :: Show a => [ (a, HopfieldData) ] -> Int -> String -> IO ()
doCheckFixed pairs index labelsName = do
    let patErrs = [ label | (label, net) <- pairs, not $ checkFixed net index]
        msg = unwords ["WARNING: The following", labelsName,
            "have produced networks where the pattern is NOT a fixed point:\n"]

    if not $ null patErrs
        then putStrLn $ msg ++ prettyList patErrs ++ "\n"
        else putStrLn "Pattern is always a fixed point\n"


-- Compute probabilities of error - i.e. pattern not fixed
doErrorProb :: Int -> Int -> [Degree] -> IO ()
doErrorProb n p degrees = do

    putStrLn $ "Expected network errors: "

    let expErrs = [ computeErrorSuperAttractorNumbers d p n | d <- degrees ]
        errorHeader = "Degree\tExpected error"

    putStrLn $ attachLabels errorHeader $ [packL degrees, packL expErrs]
