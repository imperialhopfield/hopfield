{-# LANGUAGE PatternGuards #-}

-- Provides functions to construct and measure networks with super attractors
-- with varying pattern degrees
module SuperAttractors where

import           Control.Monad
import           Control.Monad.Random (MonadRandom)
import qualified Data.Vector as V
import           Hopfield


-- List of Hopfield networks with associated super attractor degree
type Networks = [(Int, HopfieldData)]


buildSuperAttractorNetwork :: [Int] -> [Pattern] -> HopfieldData
buildSuperAttractorNetwork ns hs
  = buildHopfieldData $ concat $ zipWith replicate ns hs


powersOfTwo :: Int -> [Int]
powersOfTwo ceil = takeWhile (<=ceil) xs
  where
  	xs = 1 : map (*2) xs

-- Builds networks with the first pattern acting as a super attractor
-- i-th network contains pattern 'p' having degree 2^i, with i zero-indexed
-- Outputs (k, network), where k is the degree of the first pattern
oneSuperNetworks :: [Pattern] -> Networks
oneSuperNetworks (p:ps) = map build $ powersOfTwo n
  where
  	n       = V.length p
  	ones    = map (const 1) ps
  	build k = ( k, buildSuperAttractorNetwork (k:ones) (p:ps) )


-- Builds networks with all patterns acting as equal super attractors
-- i-th network contains patterns all having degree 2^i
allSuperNetworks :: [Pattern] -> Networks
allSuperNetworks ps = map build $ powersOfTwo n
	where
	  n       = V.length $ head ps
	  build k = ( k, buildSuperAttractorNetwork (map (const k) ps) ps )



-- Experiments to measure super attractors

p1, p2 :: V.Vector Int
p1 = V.fromList [1,1,1,-1,-1,1,1,-1,1,-1]
p2 = V.fromList [-1,-1,1,1,-1,-1,1,-1,-1,1] :: V.Vector Int


-- Networks with first pattern as a super attractor
oneSuperAttractorNets :: Networks
oneSuperAttractorNets = oneSuperNetworks [p1, p2]


-- Networks with all patterns as (equal) super attractors
allSuperAttractorsNets :: Networks
allSuperAttractorsNets = allSuperNetworks [p1, p2]


-- Measure basin of multiple networks, with various degrees
-- Returns list of tuples: (degree, basinSize)
--
-- Note: degree is not actually used in computation, but rather serves
-- as a label for each network
measureMultiBasins :: MonadRandom m => Networks -> Pattern -> m [(Int, Int)]
measureMultiBasins nets p = liftM2 zip (return ks) basinSizes
  where
    (ks, hs)   = unzip nets
    basin h    = measurePatternBasin h p
    basinSizes = sequence $ map basin hs
