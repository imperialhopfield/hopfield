module Util (
    repeatUntilEqual
) where

import Data.List

repeatUntilEqual :: (Eq a) => (a -> a) -> a -> a
repeatUntilEqual f a
  | a == new_a = a
  | otherwise  = repeatUntilEqual f new_a
  where
    new_a = f a


getString :: (Show a) => [a] -> String
getString vals = concat . intersperse ", " . map show $ vals