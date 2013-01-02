{-# LANGUAGE PatternGuards #-}

-- Provides functions to construct and measure networks with super attractors
-- with varying pattern degrees
module SuperAttractors where

import           Measurement
import           Control.Monad
import           Control.Monad.Random (MonadRandom)
import           Control.Pipe
import qualified Data.Vector as V
import           Hopfield


-- Degree of a pattern is the number of instances it has in a network
type Degree = Int


-- List containing each element in xs replicated by the corresponding ns value
-- e.g. mapReplicate [2,3] "ca" = "ccaaa"
mapReplicate :: [Degree] -> [a] -> [a]
mapReplicate ns xs
  = concat $ zipWith replicate ns xs


-- Produces all powers of two <= ceil
powersOfTwo :: Degree -> [Degree]
powersOfTwo ceil = takeWhile (<=ceil) xs
  where
    xs = 1 : map (*2) xs

-- -----------------------------------------------------------------------------
-- Combine functions. 'buildNetworks' uses these to build super attractors

-- Replicates the first pattern k times.
oneSuperAttr :: PatternCombiner [Pattern] Degree
oneSuperAttr ps k = mapReplicate (k:cycle [1]) ps


-- Replicates each pattern k times.
allSuperAttr :: PatternCombiner [Pattern] Degree
allSuperAttr ps k = mapReplicate (cycle [k]) ps


-- Aggregate list of combiner functions of input [Pattern] into a single
-- combiner function of input [[Pattern]]
aggregateCombiners :: [PatternCombiner [Pattern] Degree] -> PatternCombiner [[Pattern]] Degree
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
oneSuperNets :: Networks Degree
oneSuperNets = buildNetworks ps degrees oneSuperAttr
  where
    ps      = [p1,p2]
    degrees = powersOfTwo $ V.length $ head ps


-- Networks with all patterns as (equal) super attractors
allSuperNets :: Networks Degree
allSuperNets = buildNetworks ps degrees allSuperAttr
  where
    ps      = [p1,p2]
    degrees = powersOfTwo $ V.length $ head ps



-- Convenience function for building networks with multiple training phases
buildMultiPhaseNetwork :: [PatternCombiner [Pattern] Degree] -> Networks Degree
buildMultiPhaseNetwork combFuncs = buildNetworks patList params aggComb
  where
    patList = [ [p1,p2], [q1] ]
    params  = powersOfTwo $ (V.length . head . head) patList
    aggComb = aggregateCombiners combFuncs


retrainNormalWithOneSuper   :: Networks Degree
retrainOneSuperWithNormal   :: Networks Degree
retrainOneSuperWithOneSuper :: Networks Degree
retrainAllSuperWithNormal   :: Networks Degree
retrainAllSuperWithOneSuper :: Networks Degree

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



-- Measure basin of multiple networks, with various params
-- Returns list of tuples: (param, basinMeasure)
--
-- Note: param is not actually used in computation, but rather serves
-- as a label for each network
measureMultiBasins :: MonadRandom m => BasinMeasure m i -> Networks p -> Pattern -> Producer (p, i) m ()
measureMultiBasins measureBasin nets pat = label params <+< inPipes
  where
    (params, hs)  = unzip nets
    basin h       = measureBasin h pat
    basinMeasures = map basin hs
    inPipes       = foldl1 (>>) $ basinMeasures

    label []     = forever await
    label (d:ds) = do
      value <- await
      yield (d, value)
      label ds
