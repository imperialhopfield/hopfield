{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Utils
import Hopfield
import qualified Data.Vector as V
import Control.Monad

main = hspec $ do
  describe "base model" $ do

      describe "buildHopfieldData" $ do

         --TODO limit pattern size! After around 19 tests,
         --computer slows down to a halt due to high memory usage
         --it "trains an all-positive pattern correctly" $
         --  forAll ((sameElemVector 1) `suchThat` (not . V.null))
         --    (\pat -> weights (buildHopfieldData [pat]) == allOnesWeights (V.length pat))
         it "tests that the patterns stored in the hopfield datastructure are the same as the ones which were given as input" $
            forAll (arbitrary `suchThat` (not . null))
            (\pats -> (patterns $ buildHopfieldData pats) == pats)


