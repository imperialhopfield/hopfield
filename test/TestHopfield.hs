{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns #-}

module TestHopfield where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import qualified Data.Vector as V
import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck

import           Hopfield.Hopfield
import           Hopfield.Util
import           Hopfield.TestUtil


toV = (V.fromList <$>)
_EPSILON = 0.001


data Config = Config {
  method :: LearningType
, maxPatSize :: Int
}

configs = [ Config Hebbian 100
          , Config Storkey 30
          ]

forAllMethods testFun = forM_ configs $ \conf@Config {method, maxPatSize} ->
                          describe (show method ++ " " ++ show maxPatSize) $ do
                            testFun conf


testHopfield :: Spec
testHopfield = do
  describe "base model" $ do
    let maxPatListSize = 20
    -- let maxPatSize     = 100

    describe "buildHopfieldData " $ forAllMethods $ \(Config method maxPatSize) -> do

      -- Pattern list generator
      let patListGen'     = patListGen H maxPatSize maxPatListSize

      -- Patterns must not be empty
      let patternGenAll1 = toV . nonempty $ boundedReplicateGen maxPatSize (return 1)

      it "trains a single all-positive pattern correctly" $
        forAll patternGenAll1
          (\pat -> (list2D . weights) (buildHopfieldData method [pat]) == allWeightsSame (V.length pat))

      it "tests that the patterns stored in the hopfield datastructure are the same as the ones which were given as input" $
        forAll patListGen' (\pats -> (patterns $ buildHopfieldData method pats) == pats)

      it "tests that patterns we trained on are fixed points" $
        forAll (nonempty patListGen') $
          trainingPatsAreFixedPoints method


    describe "test repeatedUpdate" $ forAllMethods $ \(Config method maxPatSize) -> do

      return ()

      -- TODO implement
      -- it "test that when repeatedUpdate has finished, no other update can occur" $


    describe "updateChain" $ forAllMethods $ \(Config method maxPatSize) -> do

      -- Note that `updateChain` need not produce patterns like `repeatedUpdate`
      -- because the updates are random.

      it "updateChain terminates nonempty" $
        forAll (patternsTupleGen H maxPatSize maxPatListSize) $ \(training_pats, testPats) ->
          let hd = buildHopfieldData method training_pats
           in and $ flip evalRand (mkStdGen 1) $ forM testPats $ \testPat -> do
                result_chain <- updateChain hd testPat
                return $ length result_chain >= 1


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

      forAllMethods $ \(Config method maxPatSize) -> do

        it "energy decreases after doing one step" $
          forAll (patternsTupleGen H maxPatSize maxPatListSize) $ energyDecreasesAfterUpdate method
