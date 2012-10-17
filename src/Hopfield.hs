module Hopfield where

import Data.List
import Data.Vector (Vector, (!))
import Data.Vector.Generic.Mutable (write)
import qualified Data.Vector as V


type Weights = Vector (Vector Int)
type Pattern = Vector Int
type NetworkSize = Int


train :: [Pattern] -> Weights
train = undefined


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
matchPattern ws pats pat =
  case m_index of
    Nothing    -> Left converged_pattern
    Just index -> Right index
  where
    converged_pattern = repeatedUpdate ws pat
    m_index = converged_pattern `elemIndex` pats


energy :: Weights -> Pattern -> Int
energy ws pat =
  sum [ w i j * x i * x j | i <- [0 .. p-1], j <- [0 .. p-1] ]
  where
    p = V.length pat
    w i j = ws ! i ! j
    x i = pat ! i
