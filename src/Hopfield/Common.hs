{-# LANGUAGE PatternGuards, ExistentialQuantification #-}

module Hopfield.Common where


import Data.Vector (Vector)

type Weights = Vector (Vector Double)
type Pattern = Vector Int
type Bias    = Vector Double


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
