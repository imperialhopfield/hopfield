{-# LANGUAGE PatternGuards #-}

-- | Base Hopfield model, providing training and running.
module Hopfield (
    Weights
  , Pattern
  -- * Hopfield data structure
  , HopfieldData ()
  , weights
  , patterns
  , buildHopfieldData
  -- * Running
  , update
  , repeatedUpdate
  , matchPattern
  -- * Energy
  , energy
) where

import           Data.List
import           Data.Vector (Vector, (!))
import           Data.Vector.Generic.Mutable (write)
import qualified Data.Vector as V

import           Util


type Weights = Vector (Vector Double)
type Pattern = Vector Int

-- | Encapsulates the network weights together with the patterns that generate
-- it with the patterns which generate it
data HopfieldData = HopfieldData {
    weights :: Weights    -- ^ the weights of the network
  , patterns :: [Pattern] -- ^ the patterns which were used to train it
}

-- | @buildHopfieldData patterns@: Takes a list of patterns and
-- builds a Hopfield network (by training) in which these patterns are
-- stable states. The result of this function can be used to run a pattern
-- againts the network, by using 'matchPattern'.
buildHopfieldData :: [Pattern] -> HopfieldData
buildHopfieldData []   = error "Train patterns are empty"
buildHopfieldData pats
  | first_len == 0
      = error "Cannot have empty patterns"
  | not $ all (\x -> V.length x == first_len) pats
      = error "All training patterns must have the same length"
  | otherwise
      = HopfieldData (train pats) pats
  where
    first_len = V.length (head pats)


-- | @train patterns@: Trains and constructs network given a list of patterns
-- which are used to build the weight matrix. As a consequence, they will be
-- stable points in the network (by construction).
train :: [Pattern] -> Weights
train pats = vector2D ws
  -- No need to check pats ws size, buildHopfieldData does it
  where
    ws = [ [ w i j ./. p | j <- [0 .. p-1] ] | i <- [0 .. p-1] ]
    w i j
      | i == j    = 0
      | otherwise = sum [ (pat ! i) * (pat ! j) | pat <- pats ]
    p           = V.length (head pats)
    vector2D ll = V.fromList (map V.fromList ll)


-- | Same as 'update', without check, for performance.
update' :: Weights -> Pattern -> Pattern
update' ws pat =
  case updatables of
    []  -> pat
    i:_ -> V.modify (\v -> write v i (o i)) pat
  where
    updatables = [ i | (i, x_i) <- zip [1..] (V.toList pat), o i /= x_i ]
    o i        = if sum [ (ws ! i ! j) *. (pat ! j)
                        | j <- [0 .. p-1] ] >= 0 then 1 else -1
    p          = V.length pat


-- | @update weights pattern@: Applies the update rule on @pattern@ for the
-- first updatable neuron given the Hopfield network (represented by @weights@).
--
-- Pre: @length weights == length pattern@
update :: Weights -> Pattern -> Pattern
update ws pat
  | Just e <- validPattern ws pat  = error e
  | Just e <- validWeights ws      = error e
  | otherwise                      = update' ws pat


-- | @repeatedUpdate weights pattern@: Performs repeated updates on the given
-- pattern until it reaches a stable state with respect to the Hopfield network
-- (represented by @weights@).
-- Pre: @length weights == length pattern@
repeatedUpdate :: Weights -> Pattern -> Pattern
repeatedUpdate ws pat
  | Just e <- validPattern ws pat  = error e
  | Just e <- validWeights ws      = error e
  | otherwise                      = repeatUntilEqual (update' ws) pat


-- | @matchPatterns hopfieldData pattern@:
-- Computes the stable state of a pattern given a Hopfield network(represented
-- by @weights@) and tries to find a match in a list of patterns which are
-- stored in @hopfieldData@.
-- Returns:
--
--    The index of the matching pattern in @patterns@, if a match exists
--    The converged pattern (the stable state), otherwise
--
-- Pre: @length weights == length pattern@
matchPattern :: HopfieldData -> Pattern -> Either Pattern Int
matchPattern (HopfieldData ws pats) pat
  | Just e <- validPattern ws pat = error e
  | Just e <- validWeights ws = error e
  | otherwise
    = case m_index of
        Nothing    -> Left converged_pattern
        Just index -> Right index
      where
        converged_pattern = repeatedUpdate ws pat
        m_index           = converged_pattern `elemIndex` pats


-- | @energy weights pattern@: Computes the energy of a pattern given a Hopfield
-- network (represented by weights).
-- Pre: @length weights == length pattern@
energy :: Weights -> Pattern -> Double
energy ws pat
  | Just e <- validPattern ws pat = error e
  | Just e <- validWeights ws     = error e
  | otherwise = s / (-2.0)
    where
      p     = V.length pat
      w i j = ws ! i ! j
      x i   = pat ! i
      s = sum [ w i j *. (x i * x j) | i <- [0 .. p-1], j <- [0 .. p-1] ]


validPattern :: Weights -> Pattern -> Maybe String
validPattern ws pat
  | V.length ws /= V.length pat = Just "Pattern size must match network size"
  | otherwise                   = Nothing


-- | @validWeights weights@: Validates the weight matrix's correctness:
-- * It is non-empty
-- * It is square
-- * It is symmetric
-- * All diagonal elements must be zero
validWeights :: Weights -> Maybe String
validWeights ws
  | n == 0
    = Just "Weight matrix must be non-empty"
  | not $ all (\x -> V.length x == n) $ V.toList ws
    = Just "Weight matrix has to be a square matrix"
  | not $ all (== 0) [ ws ! i ! i | i <- [0..n-1] ]
    = Just "Weight matrix first diagonal must be zero"
  | not $ and [ (ws ! i ! j) == (ws ! j ! i) | i <- [0..n-1], j <- [0..n-1] ]
    = Just "Weight matrix must be symmetric"
  | otherwise = Nothing
  where
    n = V.length ws

