module Hopfield where

import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


type Weights = Vector (Vector Int)
type Pattern = Vector Int
type NetworkSize = Int


train :: [Pattern] -> Weights
train = undefined


update :: Weights -> Pattern -> Pattern
update = undefined


repeatedUpdate :: Weights -> Pattern -> Pattern
repeatedUpdate = undefined


matchPattern :: Weights -> [Pattern] -> Pattern -> Either Int Pattern
matchPattern = undefined


energy :: Weights -> Pattern -> Int
energy = undefined


lookupPattern :: Eq a => a -> [a] -> Maybe Int
lookupPattern = elemIndex
