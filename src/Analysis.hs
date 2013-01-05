module Analysis where

import           Data.List
import           Data.Number.Erf
import qualified Data.Vector as V


import Hopfield
import Util



-- TODO I don't understand this (niklas)
-- TODO check where this function is used
-- | Computes the probability of error for one element given a hopfield data
-- structure. Note that I claim that the actuall error of probability depends
-- on this, but is not the whole term
-- The assumption is that the patterns which were used to train the network
-- are independent.
computeErrorIndependentPats :: HopfieldData -> Double
computeErrorIndependentPats hopfield = computeErrorIndependentPatsNumbers p n
  where pats = patterns hopfield
        n = V.length $ pats !! 0
        p = length pats


-- | computes the error of a super attractor of a hopfield network. The assumption
-- is that there is only one super attractor and the other patterns are independent.
computeErrorSuperAttractor :: HopfieldData -> Double
computeErrorSuperAttractor hopfield = computeErrorSuperAttractorNumbers d n p
  where pats = patterns hopfield
        n = V.length $ pats !! 0
        p = length pats
        d = snd $ maximumBy (compareBy snd) (getElemOccurrences pats)


computeErrorIndependentPatsNumbers :: Int -> Int -> Double
computeErrorIndependentPatsNumbers p n
  = 1.0 / 2.0 * (1 - (erf $ sqrt $ n ./. (2 *  p)))

-- | @computeErrorSuperAttractorNumbers d p n@
-- Computes the probability of error for a super attractor with degree @d@, in
-- a Hopfield network with @n@ neurons, which has been trained with @p@ patterns.
-- The assumption is that the other patterns are independent
-- for mathematical derivation of the equation, see report.
computeErrorSuperAttractorNumbers :: Int -> Int -> Int -> Double
computeErrorSuperAttractorNumbers d p n
  = 1.0 / 2.0 * (1.0 - (erf $ (sqrt (n ./. (2 * (p - d)) ))))


-- @patternsToNeuronsRatioFromError err@. Given that the err we accept is @err@,
-- returns the maximum ratio between the number of patterns and the number of
-- neurons which can be used to ensure that the probability of error is just @err@.
-- if p/n is grater than @patternsToNeuronsRatioFromError err@ then the error
-- of a Hopfield network will be greater than err. This method is used to compute
-- the minimum number of neurons given the number of training patterns and the
-- maximum error accepted error.
patternsToNeuronsRatioFromError :: Double -> Double
patternsToNeuronsRatioFromError err = 1.0 / (2.0 * (inverf (1.0 - 2.0 * err)) ^ (2 :: Int))


-- @minNumberOfNeurons p err@ Given the number of patterns used to train a Hopfield
-- network and the maximum error accepted, returns the minimum number of neurons
-- required for the network.
minNumberOfNeurons :: Int -> Double -> Int
minNumberOfNeurons p err
  = 1 + floor (p ./ (patternsToNeuronsRatioFromError err))


maxNumberOfPatterns :: Int -> Double -> Int
maxNumberOfPatterns n err
  = floor (patternsToNeuronsRatioFromError err *. n)
