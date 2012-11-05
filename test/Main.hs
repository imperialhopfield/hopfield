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
import           Control.Monad.Random
import           Control.Applicative
import           System.Random

toV = (V.fromList <$>)
_EPSILON = 0.001


main = hspec $ do
  describe "base model" $ do
    let maxPatListSize = 20
    let maxPatSize     = 1000

    -- Pattern list generator
    let patListGen'     = patListGen maxPatSize maxPatListSize

    describe "buildHopfieldData" $ do

      -- Patterns must not be empty
      let patternGenAll1 = toV . nonempty $ boundedReplicateGen maxPatSize (return 1)

      it "trains a single all-positive pattern correctly" $
        forAll patternGenAll1
          (\pat -> (list2D . weights) (buildHopfieldData [pat]) == allWeightsSame (V.length pat))

      it "tests that the patterns stored in the hopfield datastructure are the same as the ones which were given as input" $
        forAll patListGen' (\pats -> (patterns $ buildHopfieldData pats) == pats)

      it "tests that patterns we trained on are fixed points" $
        forAll (nonempty patListGen')
          trainingPatsAreFixedPoints

    describe "update tests" $ do
      it "one update is  computed ok "

    --describe "energy tests" $ do


      it "energy is computed ok for a system with 2 neurons" $
        energy (vector2D [[0,0.5],[0.5,0]]) (V.fromList [1,0])
          `shouldBe` 0

      it "energy is computed ok for a system of 3 neurons, positive weights" $
        abs (energy (vector2D        [[0  ,0.2,0.5],
                                      [0.2,0  ,0.7],
                                      [0.5,0.7,0  ]])
                                (V.fromList [1,-1,1]) - 0.4) < _EPSILON

      it "energy is computed ok for a system of 5 neurons, positive & negative weights" $
        abs (energy (vector2D        [[ 0  , 0.2,-0.5,0.7,-0.1],
                                      [ 0.2, 0  , 0.3,0.4,-0.7],
                                      [-0.5, 0.3, 0  ,0.2,-0.4],
                                      [ 0.7, 0.4, 0.2,0  , 0.5],
                                      [-0.1,-0.7,-0.4,0.5, 0  ]
                                      ])
                                (V.fromList [1,-1,-1,1,-1]) - 0.8) < _EPSILON

      it "energy decreases after doing one step" $
        forAll (patternsTupleGen maxPatSize maxPatListSize) energyDecreasesAfterUpdate
