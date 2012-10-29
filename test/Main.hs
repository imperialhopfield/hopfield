{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Vector as V
import           Test.Hspec
import           Test.QuickCheck
import           Test.HUnit
import           Utils
import           Hopfield
import           Control.Monad

main = hspec $ do
  describe "base model" $ do

    describe "buildHopfieldData" $ do

      -- TODO limit pattern size! After around 19 tests,
      -- computer slows down to a halt due to high memory usage
      it "trains a single all-positive pattern correctly" $
        forAll ((sameElemVector 1) `suchThat` (not . V.null))
          (\pat -> weights (buildHopfieldData [pat]) == allOnesWeights (V.length pat))


       -- TODO limit pattern size!
      it "trains an arbitrary number of all-positive patterns correctly" $
        forAll (replicateGen (sameElemVector 1) `suchThat` (not . null))
          (\pats -> weights (buildHopfieldData pats) == allOnesWeights (V.length $ head pats))

         --TODO limit pattern size! After around 19 tests,
         --computer slows down to a halt due to high memory usage
      it "tests that the patterns stored in the hopfield datastructure are the same as the ones which were given as input" $
        forAll (patternListGen `suchThat` (not . null))
          (\pats -> (patterns $ buildHopfieldData pats) == pats)
