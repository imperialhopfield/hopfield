module TestMeasurement where

import           Measurement
import qualified Data.Vector as V
import           Hopfield
import           Test.Hspec
import           Test.QuickCheck
import           Test.HUnit
import           Util
import           Utils



testMeasurement = do

  describe "sampleHammingDistance" $ do

    let genPatternAndRadius = do
          p <- patternRangeGen H (1, 100)
          r <- choose (1, V.length p)
          return (p, r)


    let testHamming (p, r) = do
          let hasRDiffs p2 = r == numDiffs (V.toList p) (V.toList p2)
          samples <- evalRandGen $ sampleHammingDistance p r 25
          return $ all hasRDiffs samples

    it "generates patterns which are of the correct hamming distance" $
      forAll genPatternAndRadius testHamming
