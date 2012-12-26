module TestUtil where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit

import Data.Maybe
import Data.List

import Util
import Utils


testUtil = do
  describe "test functions in Util" $ do

    it "tests toBinary" $
      forAll buildIntTuple binaryCheck
