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
  = 1.0 / 2.0 * (1.0 - (erf $ (sqrt (n ./. (2 *. (p - d)) ) *. d)))

