{-# LANGUAGE PatternGuards #-}

module Common where


import           Data.Vector (Vector)

type Weights = Vector (Vector Double)
type Pattern = Vector Int


-- | Encapsulates the network weights together with the patterns that generate
-- it with the patterns which generate it
data HopfieldData = HopfieldData {
    weights :: Weights    -- ^ the weights of the network
  , patterns :: [Pattern] -- ^ the patterns which were used to train it
}
  deriving(Show)

