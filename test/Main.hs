{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec

import TestBoltzmann
import TestHopfield
import TestUtil
import TestMeasurement

main :: IO ()
main = hspec $ do

  testBoltzmannMachine

  testHopfield

  testUtil

  testMeasurement
