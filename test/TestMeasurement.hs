module TestMeasurement where

import           Measurement
import           Control.Monad
import           Data.List
import qualified Data.Vector as V
import           Hopfield
import qualified Math.Combinatorics.Exact.Binomial as B (choose)
import           Test.Hspec
import           Test.QuickCheck
import           Test.HUnit
import           Util
import           Utils



testMeasurement = do

  -- WARNING: Explosive complexity.
  -- Maximum pattern size parameter '10' is very sensitive
  describe "withHammingDistance" $ do

    let maxPatSize = 10
    let pats = patternRangeGen H (1, maxPatSize) :: Gen Pattern


    let testCount p r = length (withHammingDistance p r) == (n `B.choose` r)
          where n = V.length p

    it "has nCr (n choose r) patterns given n neurons and radius r" $
      forAll pats $ forAllR testCount



    let noDups p r = (length $ nub $ withHammingDistance p r)
                      == (n `B.choose` r)
          where n = V.length p

    it "has no duplicate patterns" $
      forAll pats $ forAllR noDups



    let testHamming p r = all hasRDiffs $ withHammingDistance p r
          where hasRDiffs p2 = r == numDiffs (V.toList p) (V.toList p2)

    it "generates patterns which are of the correct hamming distance" $
      forAll pats $ forAllR testHamming


-- Checks predicate 'test' holds for all hamming radii of the pattern 'p'
forAllR :: (Pattern -> Int -> Bool) -> Pattern -> Bool
forAllR test p = all (test p) [1..V.length p]
