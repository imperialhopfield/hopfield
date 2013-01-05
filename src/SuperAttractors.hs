{-# LANGUAGE PatternGuards #-}

-- Provides functions to construct and measure networks with super attractors
-- with varying pattern degrees
module SuperAttractors where

import           Measurement
import           Control.Monad.Random (MonadRandom)
import qualified Data.Vector as V
import           Hopfield


-- Degree of a pattern is the number of instances it has in a network
type Degree = Int


-- A function combining some input and given degree into patterns for a network
type PatternCombiner a = a -> Degree -> [Pattern]


-- Produces all powers of two <= ceil
powersOfTwo :: Degree -> [Degree]
powersOfTwo ceil = takeWhile (<= ceil) $ iterate (*2) 1


-- For each degree in 'ds', builds a network combining the degree and the list
-- of patterns (or some variant) 'ps' using the given function 'combine'
buildNetworks :: a -> [Degree] -> PatternCombiner a -> [HopfieldData]
buildNetworks ps ds combine
  = [ buildHopfieldData Hebbian $ combine ps d | d <- ds ]


-- -----------------------------------------------------------------------------
-- Combine functions. 'buildNetworks' uses these to build super attractors

-- Replicates the first pattern k times.
oneSuperAttr :: PatternCombiner [Pattern]
oneSuperAttr [] _      = []
oneSuperAttr (p: ps) k = replicate k p ++ ps


-- Replicates the first pattern j times, and the second pattern k times
twoSuperAttrOneFixed :: Degree -> PatternCombiner [Pattern]
twoSuperAttrOneFixed j (pa:pb: ps) k = replicate j pa ++ replicate k pb ++ ps
twoSuperAttrOneFixed _ _ _           = []


-- Replicates each pattern k times.
allSuperAttr :: PatternCombiner [Pattern]
allSuperAttr ps k = concatMap (replicate k) ps


-- Aggregate list of combiner functions of input [Pattern] into a single
-- combiner function of input [[Pattern]]
aggregateCombiners :: [PatternCombiner [Pattern]] -> PatternCombiner [[Pattern]]
aggregateCombiners combiners patList degree
  | length combiners /= length patList
      = error "Number of [Pattern] in list must match number of functions"
  | otherwise
      = concat $ zipWith ($) funcs patList
  where
    funcs = map (($ degree) . flip) combiners

-- -----------------------------------------------------------------------------
-- Experiments to measure super attractors

-- Training (pre) patterns
p1, p2 :: Pattern
p1 = V.fromList [1,1,1,-1,-1,1,1,-1,1,-1]
p2 = V.fromList [-1,-1,1,1,-1,-1,1,-1,-1,1]


-- Retraining (post) patterns
q1 :: Pattern
q1 = V.fromList [1,-1,-1,-1,1,-1,-1,1,1,1]


-- Networks with first pattern as a super attractor
oneSuperNets :: [HopfieldData]
oneSuperNets = buildNetworks ps degrees oneSuperAttr
  where
    ps      = [p1,p2]
    degrees = powersOfTwo $ V.length $ head ps


-- Networks with all patterns as (equal) super attractors
allSuperNets :: [HopfieldData]
allSuperNets = buildNetworks ps degrees allSuperAttr
  where
    ps      = [p1,p2]
    degrees = powersOfTwo $ V.length $ head ps



-- Convenience function for building networks with multiple training phases
buildMultiPhaseNetwork :: [PatternCombiner [Pattern]] -> [HopfieldData]
buildMultiPhaseNetwork combFuncs = buildNetworks patList degrees aggComb
  where
    patList = [ [p1,p2], [q1] ]
    degrees = powersOfTwo $ (V.length . head . head) patList
    aggComb = aggregateCombiners combFuncs


retrainNormalWithOneSuper   :: [HopfieldData]
retrainOneSuperWithNormal   :: [HopfieldData]
retrainOneSuperWithOneSuper :: [HopfieldData]
retrainAllSuperWithNormal   :: [HopfieldData]
retrainAllSuperWithOneSuper :: [HopfieldData]

-- A normal network (i.e. no super attractor) retrained with one super attractor
retrainNormalWithOneSuper = buildMultiPhaseNetwork [const, oneSuperAttr]

-- A network with one super attractor retrained with a normal pattern (i.e. a
-- non-super attractor)
retrainOneSuperWithNormal = buildMultiPhaseNetwork [oneSuperAttr, const]

-- A network with one super attractor retrained with another super attractor
retrainOneSuperWithOneSuper = buildMultiPhaseNetwork [oneSuperAttr, oneSuperAttr]

-- A network with all super attractors retrained with a normal pattern (i.e. a
-- non-super attractor)
retrainAllSuperWithNormal = buildMultiPhaseNetwork [allSuperAttr, const]

-- A network with all super attractors retrained with another super attractor
retrainAllSuperWithOneSuper = buildMultiPhaseNetwork [allSuperAttr, oneSuperAttr]



-- Measure basin of multiple networks
measureMultiBasins :: MonadRandom m => BasinMeasure m a -> [HopfieldData] -> Pattern -> [m a]
measureMultiBasins measureBasin hs p = map (\h -> measureBasin h p) hs

