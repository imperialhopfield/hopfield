{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec

import TestBolzmann
import TestHopfield
import TestUtil

main = hspec $ do

  testBolzmannMachine

  testHopfield

  testUtil