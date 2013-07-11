{-# LANGUAGE ScopedTypeVariables #-}

module TestBoltzmann where

import Test.Hspec
import Test.QuickCheck

import Hopfield.TestUtil


testBoltzmannMachine :: Spec
testBoltzmannMachine = do
  describe "test Boltzmann Machine" $ do

   let maxPatListSize = 20
   let maxPatSize     = 70
   let maxNrHidden  = 20

   let boltzmannBuildGen' = boltzmannBuildGen maxPatListSize maxPatSize maxNrHidden
   let boltzmannAndPatGen' = boltzmannAndPatGen maxPatListSize maxPatSize maxNrHidden

   it "tests that the patterns and nr of hidden neurons stored in the Boltzmann data structure are the same as the ones which were given as input" $
     forAll boltzmannBuildGen' build_BM_Check

   it "tests that the activation function application always gives us a probability" $
     forAll boltzmannAndPatGen' $ probabilityCheck

   it "tests that if r is 0 then the neuron always gets value 1" $
     forAll boltzmannAndPatGen' $ updateNeuronCheck 0

   it "tests that if r is 1 then the neuron always gets value 0" $
     forAll boltzmannAndPatGen' $ updateNeuronCheck 1





