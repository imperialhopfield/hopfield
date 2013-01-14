{-# LANGUAGE PatternGuards, ExistentialQuantification #-}

module Hopfield.Common where

-- This module contains data types and functions specific to the project
-- which are used for all different types of networks we support

import Data.Vector (Vector)

type Weights = Vector (Vector Double)
type Pattern = Vector Int
type Bias    = Vector Double

-- Data type used trought the project to choose a network to use
-- Boltzmann corresponds to the new method and CBoltzmann to the Classification
-- Boltzmann machine
data Method = Hopfield | Boltzmann | CBoltzmann
  deriving (Eq, Enum, Ord, Show)


-- http://www.haskell.org/haskellwiki/Heterogenous_collections
data Showable = forall a . Show a => MkShowable a

instance Show Showable
  where showsPrec p (MkShowable a) = showsPrec p a

pack :: Show a => a -> Showable
pack = MkShowable


packL :: Show a => [a] -> [Showable]
packL = map pack


-- flips a bit according to the method employed, as patterns
-- take different values if they are Hopfield or RBM.
flipBit :: Method -> Int -> Int
flipBit Hopfield  x = - x
flipBit _  x = 1 - x
