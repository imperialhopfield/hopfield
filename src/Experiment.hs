module Main where

import Control.Monad (replicateM)
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


main :: IO ()
main = do

	let n          = 25
	    numRandoms = 8
	    maxDegree  = 8


	-- The super attractor - primary care giver
	originPat <- genIO $ patternGen H n

	-- Sample random patterns - noise data
	randomPats <- replicateM numRandoms $ genIO $ patternGen H n


	let pats = originPat:randomPats

	let degrees = powersOfTwo maxDegree


	-- Original pattern as the sole pattern in the network
	putStrLn "Building networks"
	let nets = buildNetworks pats degrees oneSuperAttr


	putStrLn "Origin fixed?"
	print $ map (\n -> checkFixed n 0) $ map snd nets


	putStrLn "Test fixed"
	let (_,n1):_ = nets
	ex <- samplePatternRing n1 originPat 0
	print ex

	putStrLn "Sampling basin"
	measures <- samplePatternBasin n1 originPat
	print measures

	putStrLn "Measuring basins of attraction"
	result <- measureMultiBasins measurePatternBasin nets originPat

	-- Output: (Degree of super attractor, basin of attraction size)
	print result
