module Main where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Utils
import Hopfield
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Random
import System.Random

_EPSILON = 0.001

main = hspec $ do
  describe "base model" $ do

      --describe "buildHopfieldData" $ do

        --TODO limit pattern size! After around 19 tests,
        --computer slows down to a halt due to high memory usage
        --it "trains a single all-positive pattern correctly" $
        --  forAll ((sameElemVector 1) `suchThat` (not . V.null))
        --    (\pat -> weights (buildHopfieldData [pat]) == allOnesWeights (V.length pat))


         --TODO limit pattern size!
        --it "trains an arbitrary number of all-positive patterns correctly" $
        --  forAll (replicateGen (sameElemVector 1) `suchThat` (not . null))
        --    (\pats -> weights (buildHopfieldData pats) == allOnesWeights (V.length $ head pats))

    
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
          init_energy  = energy weights init_pattern
          final_energy = energy weights final_pattern
          weights      = (matrixToVectors [[ 0  , 0.2,-0.5,0.7,-0.1],
                                           [ 0.2, 0  , 0.3,0.4,-0.7],
                                           [-0.5, 0.3, 0  ,0.2,-0.4],
                                           [ 0.7, 0.4, 0.2,0  , 0.5],
                                           [-0.1,-0.7,-0.4,0.5, 0  ]
                                           ]) 
          init_pattern  = (V.fromList [1,-1,-1,1,-1])
          final_pattern = evalRand (update weights init_pattern) (mkStdGen 1)   
          
