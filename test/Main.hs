{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Vector as V
import           Test.Hspec
import           Test.QuickCheck
import           Test.HUnit
import           Utils
import           Hopfield
import           Util
import           Control.Monad
import           Control.Applicative

toV = (V.fromList <$>)
nonempty = (`suchThat` (not . null))

main = hspec $ do
  describe "base model" $ do

    describe "buildHopfieldData" $ do

      let maxPatListSize = 20
      let maxPatSize     = 1000

      -- Patterns must not be empty
      let patternGenAll1 = toV . nonempty $ boundedClonedGen maxPatSize (return 1)

      -- Pattern list generator
      let patListGen     = do
            i <- choose (1, maxPatSize)
            nonempty $ boundedListGen (patternGen i) maxPatListSize

      it "trains a single all-positive pattern correctly" $
        forAll patternGenAll1
          (\pat -> (list2D . weights) (buildHopfieldData [pat]) == allOnesWeights (V.length pat))


      it "trains an arbitrary number of all-positive patterns correctly" $
        forAll (nonempty $ boundedClonedGen maxPatListSize patternGenAll1)
          (\pats -> (list2D . weights) (buildHopfieldData pats) == allOnesWeights (V.length $ head pats))


      it "tests that the patterns stored in the hopfield datastructure are the same as the ones which were given as input" $
        forAll patListGen (\pats -> (patterns $ buildHopfieldData pats) == pats)
