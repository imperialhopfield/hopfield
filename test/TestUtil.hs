module TestUtil where

import Data.List
import Data.Maybe
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import Hopfield.Util
import Hopfield.TestUtil


testUtil = do
  describe "test functions in Util" $ do

    it "tests toBinary" $
      forAll buildIntTuple binaryCheck
