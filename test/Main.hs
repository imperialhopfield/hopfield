{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec

import TestBolzmann
import TestHopfield

main = hspec $ do

  testBolzmannMachine
  testHopfield