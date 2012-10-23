module Util (
    repeatUntilEqual
  , getRandom
  , (./.)
  , (*.)
) where

import           Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Random

(./.) :: (Fractional a, Integral a1, Integral a2) => a1 -> a2 -> a
x ./. y = fromIntegral x / fromIntegral y

(*.) :: (Integral a1, Num a) => a -> a1 -> a
x *. y = x * fromIntegral y


getRandom :: MonadRandom m => [a] -> m a
getRandom xs = Random.fromList (zip xs (cycle [1]))

repeatUntilEqual :: (Eq a) => (a -> a) -> a -> a
repeatUntilEqual f a
  | a == new_a = a
  | otherwise  = repeatUntilEqual f new_a
  where
    new_a = f a

