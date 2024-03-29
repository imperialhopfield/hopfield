{-# LANGUAGE ParallelListComp, ScopedTypeVariables #-}

-- | This module uses general purpose functions which are use trought the project.
-- Should not contain any project defined data types. Needs to be kept
-- as general as possible.

module Hopfield.Util (
    average
  , (*.)
  , (.*)
  , (./)
  , (./.)
  , (/.)
  , attachLabel
  , attachLabels
  , columnVector
  , combine
  , combineVectors
  , compareBy
  , dotProduct
  , findInList
  , fromDataVector
  , getBinaryIndices
  , getElemOccurrences
  , gibbsSampling
  , hammingDistance
  , list2D
  , log2
  , normal
  , numDiffs
  , prettyList
  , printMList
  , randomBinaryVector
  , randomElem
  , randomSignVector
  , repeatUntilEqual
  , repeatUntilEqualOrLimitExceeded
  , repeatUntilNothing
  , runT
  , shuffle
  , toArray
  , toBinary
  , toDouble
  , toPercents
  , vector2D
  , unfoldrSelfM
  , patternToAsciiArt
) where


import           Control.Monad
import           Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Random
import           Data.Array.ST
import           Data.List.Split (chunksOf)
import qualified Data.Random as DR
import           Data.List
import qualified Data.Vector as V
import           Data.Word (Word32)
import           Foreign.Storable
import           GHC.Arr as Arr
import qualified Numeric.Container as NC
import           Numeric.Probability.Random (T, runSeed)
import           System.Random (mkStdGen)

import           Hopfield.Common


(./.) :: (Fractional a, Integral a1, Integral a2) => a1 -> a2 -> a
x ./. y = fromIntegral x / fromIntegral y

(./) :: (Fractional a, Integral a1) => a1 -> a -> a
x ./ y = fromIntegral x / y

(/.) :: (Fractional a, Integral a2) => a -> a2 -> a
x /. y = x / fromIntegral y

(*.) :: (Integral a1, Num a) => a -> a1 -> a
x *. y = x * fromIntegral y

(.*) :: (Fractional a, Integral a1) => a1 -> a -> a
x .* y = fromIntegral x * y


toDouble :: (Integral a, Num b) => V.Vector a -> V.Vector b
toDouble = fmap fromIntegral


compareBy :: Ord b => (a -> b) -> a -> a -> Ordering
compareBy f x1 x2 = compare (f x1) (f x2)


getElemOccurrences :: Ord a => [a] -> [(a, Int)]
getElemOccurrences = map (\xs@(x:_) -> (x, length xs)) . group . sort


log2 :: Double -> Double
log2 = logBase 2.0



-- | Generates a number sampled from a random distribution, given the mean and
-- standard deviation.
normal :: forall m . MonadRandom m => Double -> Double -> m Double
normal m std = do
  r <- DR.runRVar (DR.normal m std) (Random.getRandom :: MonadRandom m => m Word32)
  return r


-- | @gibbsSampling a@ Gives the binary value of a neuron (0 or 1) from the
-- activation sum
gibbsSampling :: MonadRandom  m => Double -> m Int
gibbsSampling a
  | (a < 0.0 || a > 1.0) = error "argument of gibbsSampling is not a probability"
  | otherwise = do
      r <- Random.getRandomR (0.0, 1.0)
      return $ if (r < a) then 1 else 0


randomElem :: MonadRandom m => [a] -> m a
randomElem [] = error "randomElem: empty list"
randomElem xs = Random.fromList (zip xs (repeat 1))


repeatUntilEqual ::  (MonadRandom m, Eq a) => (a -> m a) -> a -> m a
repeatUntilEqual f a =
  do
    new_a <- f a
    if a == new_a then return a else repeatUntilEqual f new_a

repeatUntilNothing :: (MonadRandom m) => (a -> m (Maybe a)) -> a -> m a
repeatUntilNothing f x =
  do
    new_x <- f x
    case new_x of
      Nothing -> return x
      Just y  -> repeatUntilNothing f y


repeatUntilEqualOrLimitExceeded :: (MonadRandom m, Eq a) => Int -> (a -> m a) -> a -> m a
repeatUntilEqualOrLimitExceeded limit f a
  | limit < 0 = error "negative limit in repeatUntilEqualOrLimitExceeded"
  | otherwise = repeatUntilEqualOrLimitExceeded' 0 limit f a


repeatUntilEqualOrLimitExceeded' :: (MonadRandom m, Eq a) => Int -> Int -> (a -> m a) -> a -> m a
repeatUntilEqualOrLimitExceeded' current limit f a
  | current == limit = return a
  | otherwise = do
      new_a <- f a
      if a == new_a then return a else repeatUntilEqualOrLimitExceeded' (current + 1) limit f new_a


-- | Converts a list of lists to a 2D vector
vector2D :: [[a]] -> V.Vector (V.Vector a)
vector2D ll = V.fromList $ map V.fromList ll


-- | Converts a 2D vector into a list of lists
list2D :: V.Vector (V.Vector a) -> [[a]]
list2D vv = map V.toList $ V.toList vv

-- Returns the coumn vector of a matrix
-- Caller needs to ensure that the matrix is well formed
columnVector :: V.Vector (V.Vector a) -> Int -> V.Vector a
columnVector m i = V.map (V.! i) m


-- from Data.Vector to Numeric.Container.Vector
fromDataVector::  (Foreign.Storable.Storable a) => V.Vector a -> NC.Vector a
fromDataVector v = NC.fromList $ V.toList v

-- the caller has to ensure that the dimensions are the same
combine :: (a-> b -> c) -> [[a]] -> [[b]] -> [[c]]
combine f xs ys
  | length xs /= length ys = error "list sizes do not match in Utils.combine"
  | otherwise = zipWith (zipWith f) xs ys


combineVectors :: (a -> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
combineVectors f v_a v_b
  | V.length v_a /= V.length v_b = error "vector sizes do not match in dot product"
  | otherwise = V.fromList (zipWith f (V.toList v_a) (V.toList v_b) )


-- assertion same size and move to Util
dotProduct :: Num a => V.Vector a -> V.Vector a -> a
dotProduct xs ys
  | V.length xs /= V.length ys = error "vector sizes do not match in dot product"
  | otherwise = sum [ xs V.! i * (ys V.! i ) | i <- [0.. V.length xs - 1]]


-- Tries to find a element in a list. In case of success, returns the index
-- of the element (the first one, in case of multiple occurences). In case of
-- failure, returns the search element itself.
findInList :: Eq a => [a] -> a -> Either a Int
findInList xs x =
  case m_index of
        Nothing -> Left x
        Just i  -> Right i
  where m_index = x `elemIndex` xs


-- @toBinary n size@. Returns the binary representation of n in size bits.
-- The caller has to ensure that n fits in size bits, or an error will be raised.
toBinary :: Int -> Int -> [Int]
toBinary n size
  | n < 0             = error "toBinary requires positive arguments"
  | n >  2 ^ size - 1 = error "cannot fit binary representation into given size"
  | otherwise =  [ (n `div` 2 ^ i) `mod` 2 | i <- [size - 1, size - 2 .. 0] ]

-- returns the binary represenation of the indices of the elements in a list
-- after the duplicates have been removed
getBinaryIndices :: Eq a => [a] -> [(a, [Int])]
getBinaryIndices xs = [ (x, toBinary i bitsNeeded) | i <- [0 ..] | x <- nub_xs]
  where
    nub_xs = nub xs
    bitsNeeded = 1 + (floor $ log2 $ fromIntegral (length nub_xs)) :: Int


-- Counts the number of pairwise differences in two lists
numDiffs :: (Eq a) => [a] -> [a] -> Int
numDiffs xs ys = length $ filter id $ zipWith (/=) xs ys


-- Convert list to Array
toArray :: [a] -> Array Int a
toArray xs = listArray (0, l-1) xs
  where l = length xs


-- Efficient O(n) random shuffle of an array
-- Modified from http://www.haskell.org/haskellwiki/Random_shuffle
shuffle :: MonadRandom m => Array Int a -> m [a]
shuffle xs = do
    let len = Arr.numElements xs
    rands <- take len `liftM` Random.getRandomRs (0, len-1)
    let shuffledArray = runSTArray $ do
                ar <- Arr.thawSTArray xs
                forM_ (zip [0..(len-1)] rands) $ \(i, j) -> do
                    vi <- Arr.readSTArray ar i
                    vj <- Arr.readSTArray ar j
                    Arr.writeSTArray ar j vi
                    Arr.writeSTArray ar i vj
                return ar
    return (elems shuffledArray)


-- Run a random generator T (Numeric.Probability.Random) in MonadRandom
runT :: forall m a . MonadRandom m => T a -> m a
runT dist = do
  rndInt <- Random.getRandom
  return $ runSeed (mkStdGen rndInt) dist


randomBinaryVector :: MonadRandom m => Int -> m (V.Vector Int)
randomBinaryVector size = liftM V.fromList $ replicateM size $ Random.getRandomR (0, 1)


randomSignVector :: MonadRandom m => Int -> m (V.Vector Int)
randomSignVector size = do
  binaryVec <- randomBinaryVector size
  return $ V.map (\x -> 2 * x - 1) binaryVec


-- Returns the average of the elements in a list
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs


hammingDistance :: V.Vector Int -> V.Vector Int -> Int
hammingDistance p1 p2 = length $ filter (== -1) $ zipWith (*) l1 l2
  where [l1, l2] = map V.toList [p1, p2]


-- Convert lists of double to a pretty string of (rounded) percentages
-- e.g. toPercents [0.123, 0.999] = "12% 99%"
toPercents :: [Double] -> String
toPercents ns = unwords [ show (round $ n * 100.0 :: Int) ++ "%" | n <- ns]


-- Prints given elements separated by a tab
attachLabel :: [Showable] -> String
attachLabel xs = concat $ intersperse "\t" $ map show xs


-- Tabulates the two given lists as columns
attachLabels :: String -> [[Showable]] -> String
attachLabels header is
  = header ++ "\n" ++ concat list
  where list  = [ attachLabel i ++ "\n" | i <- is ]


-- Format list for output
prettyList :: Show a => [a] -> String
prettyList xs = unwords $ map show xs


-- Prints a list of IO actions, applying a corresponding function to it
-- e.g. printMList [IO a1, IO a2] [f1, f2]
-- Outputs the equivalent of:
-- show f1 a1 ++ show f2 a2
printMList :: (Show a) => [IO a] -> [a -> String] -> IO ()
printMList [] _          = return ()
printMList _ []          = error "Function list shorter than IO action list"
printMList (x:xs) (f:fs) = do
    value <- x
    putStrLn $ f value
    printMList xs fs


-- | Executes the monadic action returning a maybe until 'Nothing' is returned,
-- collecting the results in a list.
--
-- Like `unfoldr`, the initial value is not part of the result list.
unfoldrSelfM :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
-- Could be the following with `unfoldrM` from monad-loops:
--   unfoldrSelfM f seed = unfoldrM (\x -> ((\z -> (z,z)) <$>) `liftM` f x) seed
-- but monad-loops < 0.4.2 has a bug:
--   https://github.com/mokus0/monad-loops/commit/7ede550ecd2df61d12f5148b86bd5f3daaf6eb24
unfoldrSelfM f seed = go seed
  where
    go a = do
      mx <- f a
      case mx of
        Nothing -> return []
        Just x  -> do xs <- go x
                      return $ x : xs


patternToAsciiArt :: Int -> Pattern -> String
patternToAsciiArt width = unlines . chunksOf width . V.toList . fmap toChar
  where
    toChar i | i > 0     = '1'
             | otherwise = ' '
