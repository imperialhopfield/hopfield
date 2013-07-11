{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec

import TestBoltzmann
import TestHopfield
import TestBinary
import TestMeasurement

main :: IO ()
main = hspec $ do

  testBoltzmannMachine

  testHopfield

  testBinary

  testMeasurement
