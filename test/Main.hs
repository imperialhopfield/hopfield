{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Utils
import Hopfield
import qualified Data.Vector as V

main = hspec $ do
  describe "base model" $ do

      describe "buildHopfieldData" $ do

         it "trains an all-positive pattern correctly" $
           forAll ((sameElemVector 1) `suchThat` (not . V.null))
             (\pat -> weights (buildHopfieldData [pat]) == allOnesWeights (V.length pat))
