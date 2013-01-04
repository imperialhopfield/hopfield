{-# LANGUAGE ScopedTypeVariables #-}

module TestHopfield where

import qualified Data.Vector as V
import           Test.Hspec
import           Test.QuickCheck
import           Test.HUnit
import           Utils
import           Hopfield
import           Util
import           Control.Monad.Random
import           Control.Applicative


toV = (V.fromList <$>)
_EPSILON = 0.001


testHopfield :: Spec
testHopfield = do
  describe "base model" $ do
    let maxPatListSize = 20
    let maxPatSize     = 100

    -- Pattern list generator
    let patListGen'     = patListGen H maxPatSize maxPatListSize

    describe "buildHopfieldData Hebbian" $ do
    -- Patterns must not be empty
      let patternGenAll1 = toV . nonempty $ boundedReplicateGen maxPatSize (return 1)

      it "trains a single all-positive pattern correctly" $
        forAll patternGenAll1
          (\pat -> (list2D . weights) (buildHopfieldData Hebbian [pat]) == allWeightsSame (V.length pat))

      it "tests that the patterns stored in the hopfield datastructure are the same as the ones which were given as input" $
        forAll patListGen' (\pats -> (patterns $ buildHopfieldData Hebbian pats) == pats)

      it "tests that patterns we trained on are fixed points" $
        forAll (nonempty patListGen') $
          trainingPatsAreFixedPoints Hebbian


    describe "test repeatedUpdate" $ do
      it "test that when repeatedUpdate has finished, no other update can occur" $
        forAll (patternsTupleGen H maxPatSize maxPatListSize) $ energyDecreasesAfterUpdate Hebbian

    describe "getUpdatables" $ do
      it "getUpdatables test1" $
        getUpdatables (vector2D  [[0  ,0.2,0.5],
                                [0.2,0  ,0.7],
                                [0.5,0.7,0  ]]) (V.fromList [1, 1, -1])
        `shouldBe` ([0, 1, 2])

      it "getUpdatables test2" $
        getUpdatables (vector2D [[ 0  , 0.2,-0.5,0.7,-0.1],
                           [ 0.2, 0  , 0.3,0.4,-0.7],
                           [-0.5, 0.3, 0  ,0.2,-0.4],
                           [ 0.7, 0.4, 0.2,0  , 0.5],
                           [-0.1,-0.7,-0.4,0.5, 0  ]
                           ]) (V.fromList [1,-1,-1,1,-1])
        `shouldBe` ([1, 3, 4])

    describe "matchPattern tests" $ do
      let check pats p = evalRand (matchPattern
                                     (buildHopfieldData Hebbian $ V.fromList <$> pats)
                                     (V.fromList p))
                                  (mkStdGen 1)
          y `givesIndex` x = y `shouldBe` (Right x)
          y `givesPattern` p = y `shouldBe` (Left $ V.fromList p)

      it "matchPattern test for one pattern, giving the same pattern for recognion" $
        check [[1, 1, 1]] [1, 1, 1] `givesIndex` 0

      it "matchPattern test for one pattern, giving the opposite pattern for recognion" $
        check [[1, 1, 1]] [-1, -1, -1]  `givesPattern` [-1, -1, -1]

      it "matchPattern test for one pattern, giving the opposite pattern for recognion" $
        check [[1, 1, 1], [-1, 1, -1]] [1, 1, 1] `givesIndex` 0

      it "matchPattern test for one pattern, giving the opposite pattern for recognion" $
        check [[1, 1, 1], [-1, 1, -1]] [-1, 1, -1] `givesIndex` 1

      it "matchPattern test for one pattern, giving the opposite pattern for recognion" $
        check [[1, 1, 1], [-1, 1, -1]] [-1, -1, -1] `givesIndex` 1

    describe "energy tests" $ do
      it "energy is computed ok for a system with 2 neurons" $
        energy (vector2D [[0,0.5],[0.5,0]]) (V.fromList [1, -1])
          `shouldBe` 0.5

      it "energy is computed ok for a system of 3 neurons, positive weights" $
        abs (energy (vector2D [[0  ,0.2,0.5],
                               [0.2,0  ,0.7],
                               [0.5,0.7,0  ]])
                                (V.fromList [1,-1,1]) - 0.4) < _EPSILON

      it "energy is computed ok for a system of 5 neurons, positive & negative weights" $
        abs (energy (vector2D [[ 0  , 0.2,-0.5,0.7,-0.1],
                               [ 0.2, 0  , 0.3,0.4,-0.7],
                               [-0.5, 0.3, 0  ,0.2,-0.4],
                               [ 0.7, 0.4, 0.2,0  , 0.5],
                               [-0.1,-0.7,-0.4,0.5, 0  ]
                               ])
                                (V.fromList [1,-1,-1,1,-1]) - 0.8) < _EPSILON

      it "energy decreases after doing one step" $
        forAll (patternsTupleGen H maxPatSize maxPatListSize) $ energyDecreasesAfterUpdate Hebbian
