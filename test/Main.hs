{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec

import TestBolzmann
import TestHopfield
import TestUtil
import TestMeasurement

main :: IO ()
main = hspec $ do

  testBolzmannMachine

  testHopfield

  testUtil

  testMeasurement
