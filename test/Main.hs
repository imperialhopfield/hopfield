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
nonempty = (`suchThat` (not . null))
_EPSILON = 0.001


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


      it "tests that patterns we trained on are fixed points" $
        forAll (nonempty patListGen)
          trainingPatsAreFixedPoints

    describe "energy tests" $ do

      it "energy is computed ok for small system, 2 neurons" $
        energy (matrixToVectors [[0,0.5],[0.5,0]]) (V.fromList [1,0])
          `shouldBe` 0

      it "energy is computed ok for small system, 3 neurons, positive weights" $
        abs (energy (matrixToVectors [[0  ,0.2,0.5],
                                      [0.2,0  ,0.7],
                                      [0.5,0.7,0  ]])
                                (V.fromList [1,-1,1]) - 0.4) < _EPSILON

      it "energy is computed ok for large system, 5 neurons, positive & negative weights" $
        abs (energy (matrixToVectors [[ 0  , 0.2,-0.5,0.7,-0.1],
                                      [ 0.2, 0  , 0.3,0.4,-0.7],
                                      [-0.5, 0.3, 0  ,0.2,-0.4],
                                      [ 0.7, 0.4, 0.2,0  , 0.5],
                                      [-0.1,-0.7,-0.4,0.5, 0  ]
                                      ])
                                (V.fromList [1,-1,-1,1,-1]) - 0.8) < _EPSILON

      it "energy is decreasing after doing one step, large system" $
        init_energy > final_energy
        where
        init_energy  = energy ws init_pattern
        final_energy = energy ws final_pattern
        ws           = (matrixToVectors [[ 0  , 0.2,-0.5,0.7,-0.1],
                                         [ 0.2, 0  , 0.3,0.4,-0.7],
                                         [-0.5, 0.3, 0  ,0.2,-0.4],
                                         [ 0.7, 0.4, 0.2,0  , 0.5],
                                         [-0.1,-0.7,-0.4,0.5, 0  ]
                                         ])
        init_pattern  = (V.fromList [1,-1,-1,1,-1])
        final_pattern = evalRand (update ws init_pattern) (mkStdGen 1)
