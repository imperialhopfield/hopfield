{-# LANGUAGE PatternGuards #-}

module Common where


import Data.Vector (Vector)

type Weights = Vector (Vector Double)
type Pattern = Vector Int
type Bias    = Vector Double


data Method = Hopfield | Boltzmann | CBoltzmann
  deriving (Eq, Enum, Ord, Show)
