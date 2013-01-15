module TestUtil where

import Test.Hspec
import Test.QuickCheck

import Hopfield.TestUtil


testUtil = do
  describe "test functions in Util" $ do

    it "tests toBinary" $
      forAll buildIntTuple binaryCheck
