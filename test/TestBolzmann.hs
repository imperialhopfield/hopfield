{-# LANGUAGE ScopedTypeVariables #-}

module TestBolzmann where

import qualified Data.Vector as V
import           Test.Hspec
import           Test.QuickCheck
import           Test.HUnit
import           Utils
import           Util
import           Control.Monad
import           Control.Monad.Random
import           Control.Applicative
import           System.Random

testBolzmannMachine = do
  describe "test Bolzmann Machine" $ do

    let maxPatListSize = 20
    let maxPatSize     = 70
    let maxNrHidden  = 20

    let bolzmannBuildGen' = bolzmannBuildGen maxPatListSize maxPatSize maxNrHidden
    let bolzmannAndPatGen' = bolzmannAndPatGen maxPatListSize maxPatSize maxNrHidden

    it "tests that the patterns and nr of hidden neurons stored in the bolzmann datastructure are the same as the ones which were given as input" $
      forAll bolzmannBuildGen' build_BM_Check

    it "tests that the activation funcion application always gives us a probability"
      forAll bolzmannAndPatGen' $ probabilityCheck

    it "tests that if r is 0 then the neuron always gets value 1" $
      forAll bolzmannAndPatGen' $ updateNeuronCheck 0

    it "tests that if r is 1 then the neuron always gets value 0" $
      forAll bolzmannAndPatGen' $ updateNeuronCheck 1





