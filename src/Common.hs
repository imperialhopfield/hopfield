{-# LANGUAGE PatternGuards #-}

module Common where


import           Data.List
import           Data.Maybe
import           Data.Number.Erf
import           Data.Vector (Vector, (!))
import           Data.Vector.Generic.Mutable (write)
import qualified Data.Vector as V
import           Control.Monad.Random (MonadRandom)
import           Util

type Weights = Vector (Vector Double)
type Pattern = Vector Int


-- | Encapsulates the network weights together with the patterns that generate
-- it with the patterns which generate it
data HopfieldData = HopfieldData {
    weights :: Weights    -- ^ the weights of the network
  , patterns :: [Pattern] -- ^ the patterns which were used to train it
}
  deriving(Show)

