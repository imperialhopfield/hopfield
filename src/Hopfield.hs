module Hopfield where

import Data.List
import Data.Vector (Vector, (!))
import Data.Vector.Generic.Mutable (write)
import qualified Data.Vector as V


type Weights = Vector (Vector Int)
type Pattern = Vector Int
type NetworkSize = Int


train :: [Pattern] -> Weights
train pats
  | not $ validPatternSet pats = error "Invalid patterns"
  | otherwise                  = vector2D weights
  where
    vector2D ll = V.fromList (map V.fromList ll)
    neurons     = V.length (head pats)
    w i j
      | i == j    = 0
      | otherwise = sum [ (p ! i) * (p ! j) | p <- pats ]
    weights = [ [ w i j | j <- [0 .. neurons-1] ] | i <- [0 .. neurons-1] ]


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


repeatedUpdate :: Weights -> Pattern -> Pattern
repeatedUpdate ws pat
  | new_pat == pat = pat
  | otherwise      = repeatedUpdate ws new_pat
  where
    new_pat = update ws pat


matchPattern :: Weights -> [Pattern] -> Pattern -> Either Pattern Int
matchPattern ws pats pat
  | not $ compatibleWeightsPatternSetMatchPattern ws pats pat
    = error "Invalid or incompatible arguments"
  | otherwise
    = case m_index of
        Nothing    -> Left converged_pattern
        Just index -> Right index
      where
        converged_pattern = repeatedUpdate ws pat
        m_index           = converged_pattern `elemIndex` pats


energy :: Weights -> Pattern -> Int
energy ws pat
  | not $ compatibleWeightsPattern ws pat
    = error "Pattern incompatible with weight matrix"
  | otherwise
    = sum [ w i j * x i * x j | i <- [0 .. p-1], j <- [0 .. p-1] ] `div` (-2)
    where
      p     = V.length pat
      w i j = ws ! i ! j
      x i   = pat ! i


-- | @allTheSame xs@: Predicate determining whether all elements are the same
allTheSame :: (Eq a) => [a] -> Bool
allTheSame []     = True
allTheSame (x:xs) = all (== x) xs


-- | @nonEmptyPattern pattern@: Ensures that the pattern is non-empty
nonEmptyPattern :: Pattern -> Bool
nonEmptyPattern pat
  | V.length pat == 0 = error "Pattern must be non-empty"
  | otherwise         = True


-- | @validPatternSet patterns@: Validates the pattern set's correctness:
--
-- * We have at least one pattern
--
-- * All patterns are non-empty
--
-- * All patterns are of the same length
--
-- Throws an error if the patterns are invalid.
validPatternSet :: [Pattern] -> Bool
validPatternSet [] = error "Must have at least one training pattern"
validPatternSet pats
  | not $ all nonEmptyPattern pats
    = error "Pattern list contains invalid pattern"
  | head patLengths == 0
    = error "Empty patterns are not allowed"
  | not $ allTheSame patLengths
    = error "All patterns must have the same length"
  | otherwise = True
  where
    patLengths  = map V.length pats


-- | @validWeights weights@: Validates the weight matrix's correctness:
--
-- * It is non-empty
--
-- * It is square
--
-- * It is symmetric
--
-- * All diagonal elements must be zero
--
-- Throws an error if the weight matrix is invalid.
validWeights :: Weights -> Bool
validWeights ws
  | n == 0
    = error "Weight matrix must be non-empty"
  | not $ allTheSame $ [n] ++ (map V.length $ V.toList ws)
    = error "Weight matrix must be square"
  | not $ all (== 0) [ ws ! i ! i | i <- [0..n-1]]
    = error "Weight matrix diagonal must be zero"
  | not $ and [ (ws ! i ! j) == (ws ! j ! i) | i <- [0..n-1], j <- [0..n-1]]
    = error "Weight matrix must be symmetric"
  | otherwise = True
  where
    n = V.length ws


-- | @compatibleWeightsPattern weights pattern@: Checks that the the pattern is
-- compatible with the weight matrix - i.e. has the same dimension
compatibleWeightsPattern :: Weights -> Pattern -> Bool
compatibleWeightsPattern ws pat = V.length ws == V.length pat


-- | @compatibleWeightsPatternSet weights patterns@: Checks that the the
-- patterns are compatible with the weight matrix - i.e. all have the same
-- dimension as it
compatibleWeightsPatternSet :: Weights -> [Pattern] -> Bool
compatibleWeightsPatternSet ws pats = all (==n) patLengths
  where
    n          = V.length ws
    patLengths = map V.length pats


-- | @compatibleWeightsPatternSetMatchPattern weights patterns pattern@:
-- Checks that the weight matrix, pattern list, and match pattern are valid and
-- compatible in dimension.
compatibleWeightsPatternSetMatchPattern :: Weights -> [Pattern] -> Pattern -> Bool
compatibleWeightsPatternSetMatchPattern ws pats pat
  | not $ validWeights ws
    = error "Invalid weight matrix"
  | not $ nonEmptyPattern pat
    = error "Invalid match pattern"
  | not $ validPatternSet pats
    = error "Invalid pattern list"
  | not $compatibleWeightsPattern ws pat
    = error "Pattern to match against is incompatible with the weight matrix"
  | not $compatibleWeightsPatternSet ws pats
    = error "Patterns in pattern list are incompatible with the weight matrix"

--TODO [oct 20]
-- rename Weights and PatternSet to new names on merge with Mihaela's code
-- (the one adding comments and the new HopfieldData type)
