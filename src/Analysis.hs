module Analysis where


import           Data.Number.Erf
import qualified Data.Vector as V


import Hopfield
import Util



-- TODO I don't understand this (niklas)
-- TODO check where this function is used
-- | Computes the probability of error for one element given a hopfield data
-- structure. Note that I claim that the actuall error of probability depends
-- on this, but is not the whole term
-- computeErrorIndependentPats :: HopfieldData -> Double
-- computeErrorIndependentPats (HopfieldData _ pats) = 1.0 / 2.0 * (1 - (erf $ sqrt $ n ./. (2 *  p)))
  -- where n = V.length $ pats !! 0
        -- p = length pats


-- computeErrorSuperAttractor :: Hopefield -> Double
-- computeErrorSuperAttractor (HopfieldData _pats)

-- | Computes the probability of error for a super attractor with degree d, in
-- a Hopfield network with n neurons, which has been used to
-- The assumption is that the other patterns are independent
-- for mathematical derivation of the equation, see report.
computeErrorSuperAttractor :: Int -> Int -> Int -> Double
computeErrorSuperAttractor d p n
  = 1.0 / 2.0 * (1.0 - (erf $ (sqrt (n ./. 2 *. (p - d) ) *. d)))

