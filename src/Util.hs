module Util (
    repeatUntilEqual
  , getString
  , (./.)
  , (*.)
) where

import Data.List


(./.) :: (Fractional a, Integral a1, Integral a2) => a1 -> a2 -> a
x ./. y = fromIntegral x / fromIntegral y

(*.) :: (Integral a1, Num a) => a -> a1 -> a
x *. y = x * fromIntegral y


repeatUntilEqual :: (Eq a) => (a -> a) -> a -> a
repeatUntilEqual f a
  | a == new_a = a
  | otherwise  = repeatUntilEqual f new_a
  where
    new_a = f a


getString :: (Show a) => [a] -> String
getString vals = concat . intersperse ", " . map show $ vals
