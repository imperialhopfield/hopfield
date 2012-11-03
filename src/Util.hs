module Util (
    repeatUntilEqual
  , randomElem
  , vector2D
  , (./.)
  , (*.)
) where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Random

(./.) :: (Fractional a, Integral a1, Integral a2) => a1 -> a2 -> a
x ./. y = fromIntegral x / fromIntegral y

(*.) :: (Integral a1, Num a) => a -> a1 -> a
x *. y = x * fromIntegral y


randomElem :: MonadRandom m => [a] -> m a
randomElem [] = error "randomElem: empty list"
randomElem xs = Random.fromList (zip xs (repeat 1))


repeatUntilEqual ::  (MonadRandom m, Eq a) => (a -> m a) -> a -> m a
repeatUntilEqual f a =
  do
    new_a <- f a
    if a == new_a then return a else repeatUntilEqual f new_a


-- | Converts a list of lists to a 2D vector
vector2D :: [[a]] -> Vector (Vector a)
vector2D ll = V.fromList (map V.fromList ll)
