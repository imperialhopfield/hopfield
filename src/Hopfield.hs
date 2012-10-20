-- | Base Hopfield model, providing training and running.
module Hopfield (
    Weights
  , Pattern
  -- * Hopfield data structure
  , HopfieldData ()
  , buildHopfieldData
  -- * Running
  , update
  , repeatedUpdate
  , matchPattern
  -- * Energy
  , energy
) where

import Data.List
import Data.Vector (Vector, (!))
import Data.Vector.Generic.Mutable (write)
import qualified Data.Vector as V


type Weights = Vector (Vector Int)
type Pattern = Vector Int

data HopfieldData = HopfieldData Weights [Pattern]

-- | @buildHopfieldData patterns@: Takes a list of patterns and
-- builds a Hopfield network (by training) in which these patterns are
-- stable states.
buildHopfieldData :: [Pattern] -> HopfieldData
buildHopfieldData pats = HopfieldData weigths pats
  where
    weigths = train pats


-- | @train pattern@: Trains and construct network given a list of patterns
-- which are then stored in the network. These patterns will be stable points in
-- the network (by construction).
train :: [Pattern] -> Weights
train [] = V.fromList []
train pats = vector2D weights
  where
    vector2D ll = V.fromList (map V.fromList ll)
    neurons = V.length (head pats)
    w i j
      | i == j    = 0
      | otherwise = sum [ (p ! i) * (p ! j) | p <- pats ]
    weights = [ [ w i j | j <- [0 .. neurons-1] ] | i <- [0 .. neurons-1] ]


-- | @update weights pattern@: Applies the update rule on @pattern@ for a random
-- updatable neuron given the Hopfield network (represented by @weights@).
--
-- Pre: @length weights == length pattern@
update :: Weights -> Pattern -> Pattern
update ws pat =
  case updatables of
    []  -> pat
    i:_ -> V.modify (\v -> write v i (o i)) pat
  where
    updatables = [ i | (i, x_i) <- zip [1..] (V.toList pat), o i /= x_i ]
    o i        = if sum [ (ws ! i ! j) * (pat ! j)
                        | j <- [0 .. p-1] ] >= 0 then 1 else -1
    p          = V.length pat


-- | @repeatedUpdate weights pattern@: Performs repeated updates on the given
-- pattern until it reaches a stable state with respect to the Hopfield network
-- (represented by @weights@).
-- Pre: @length weights == length pattern@
repeatedUpdate :: Weights -> Pattern -> Pattern
repeatedUpdate ws pat
  | new_pat == pat = pat
  | otherwise      = repeatedUpdate ws new_pat
  where
    new_pat = update ws pat


-- | @matchPatterns (HopfieldData weights pattterns) pattern@:
-- Computes the stable state of a pattern given a Hopfield network(represented
-- by @weights@) and tries to find a match in a list of patterns (@patterns@).
-- Returns:

--    The index of the matching pattern in @patterns@, if a match exists
--    The converged pattern (the stable state), otherwise
--
-- Pre: @length weights == length pattern@
matchPattern :: HopfieldData -> Pattern -> Either Pattern Int
matchPattern (HopfieldData ws pats) pat =
  case m_index of
    Nothing    -> Left converged_pattern
    Just index -> Right index
  where
    converged_pattern = repeatedUpdate ws pat
    m_index = converged_pattern `elemIndex` pats


-- | @enjoy weights pattern@: Computes the energy of a pattern given a Hopfield
-- network (represented by weigths)
-- Pre: @length weights == length pattern@
energy :: Weights -> Pattern -> Int
energy ws pat =
  sum [ w i j * x i * x j | i <- [0 .. p-1], j <- [0 .. p-1] ]
  where
    p = V.length pat
    w i j = ws ! i ! j
    x i = pat ! i
