module TestBinary where

import Test.Hspec
import Test.QuickCheck

import Hopfield.TestUtil

testBinary :: Spec
testBinary = do
  describe "test functions in Util" $ do

    it "tests toBinary" $
      forAll buildIntTuple binaryCheck
