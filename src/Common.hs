{-# LANGUAGE PatternGuards #-}

module Common where


import Data.Vector (Vector)

type Weights = Vector (Vector Double)
type Pattern = Vector Int
type Bias    = Vector Double


data Method = Hopfield | Boltzmann | CBoltzmann
  deriving (Eq, Enum, Ord, Show)


-- flips a bit according to the method employed, as patterns
-- take different values if they are Hopfield or RBM.
flipBit :: Method -> Int -> Int
flipBit Hopfield  x = - x
flipBit _  x = 1 - x
