module Utils where

import qualified Data.Vector as V
import Test.QuickCheck
import Control.Monad

-- Defines an arbitrary vector
instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary


-- Convert a list generator to a vector generator
toGenVector :: Gen [a] -> Gen (V.Vector a)
toGenVector list = liftM V.fromList list


-- Generate lists containing only 'n'
sameElemList :: a -> Gen [a]
sameElemList n = do
  len <- arbitrary
  return $ replicate len n


-- Generate vectors containing only 'n'
sameElemVector :: a -> Gen (V.Vector a)
sameElemVector = toGenVector . sameElemList


-- Produces a matrix with 0's along the diagonal and 1's otherwise
allOnesWeights :: Int -> (V.Vector (V.Vector Double))
allOnesWeights n
  = V.fromList $ map V.fromList [ replaceAtN i 0 ones | i <- [0..n-1] ]
    where
      ones = replicate n 1


-- Replaces the nth element in the list with 'r'
replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ []     = error "index greater than list size"
replaceAtN 0 r (x:xs) = (r:xs)
replaceAtN n r (x:xs)
  | n > 0     = (x:(replaceAtN (n-1) r xs))
  | otherwise = error "negative index"
