{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Base Hopfield model, providing training and running.
module Hopfield (
    Pattern
  , Weights
  , LearningType (Hebbian, Storkey)
  -- * Hopfield data structure
  , HopfieldData ()
  , weights
  , patterns
  , buildHopfieldData
  -- * Running
  , update
  , getUpdatables
  , updateViaIndex
  , repeatedUpdate
  , matchPattern
  , computeH
  -- * Energy
  , energy
) where

import           Control.Monad.Random (MonadRandom)
import           Data.Maybe
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Data.Vector.Generic.Mutable (write)
import           Common
import           Util



data LearningType = Hebbian | Storkey deriving (Eq, Show)

--make Hopefield data implement show
-- | Encapsulates the network weights together with the patterns that generate
-- it with the patterns which generate it
data HopfieldData = HopfieldData {
    weights :: Weights    -- ^ the weights of the network
  , patterns :: [Pattern] -- ^ the patterns which were used to train it
} deriving (Show)


-- | Checks if weights and pattern given to the function satisfy their constraints,
-- if yes, calling the function, otherwise erroring out.
-- Usage: `checkWsPat (functionTakingWeightsAndPattern)`.
checkWsPat :: (Weights -> Pattern -> a) -> Weights -> Pattern -> a
checkWsPat f ws pat
  | Just e <- validWeights ws                = error e
  | Just e <- validPattern pat               = error e
  | Just e <- validWeightsPatternSize ws pat = error e
  | otherwise                                = f ws pat


-- | @update weights pattern@: Applies the update rule on @pattern@ for the
-- first updatable neuron given the Hopfield network (represented by @weights@).
--
-- Pre: @length weights == length pattern@
update :: MonadRandom m => Weights -> Pattern -> m Pattern
update = checkWsPat update_


-- TODO Mihaela what valid?
-- | @getUpdatables ws pat@. Given a Hopfield network represented by ws, returns
-- a list of pairs comprising of the updatable neurons (represented by the index
-- in the pattern) and their new, changed value.
-- No check is performed in this function for efficiency reasons: the checks
-- are expensive and are done in update, before update_.
-- Any other caller should ensure that ws and pat are compatible and valid
-- (by calling @valid@)
getUpdatables :: Weights -> Pattern -> [(Int, Int)]
getUpdatables = checkWsPat getUpdatables_


-- | @updateViaIndex updatables index pat@ Takes the new value of the neuron
-- represented by @index@ and changes its value in pat, returning the
-- changed, updated pattern.
-- The caller must ensure that index is smalupdateViaIndex updatables index patler than the length of updatables
updateViaIndex :: [(Int, Int)] -> Int -> Pattern -> Pattern
updateViaIndex updatables index pat
  | Just e <- validPattern pat    = error e
  -- TODO Mihaela this one is for you: index in updatables check
  | otherwise                     = updateViaIndex_ updatables index pat


-- | @repeatedUpdate weights pattern@: Performs repeated updates on the given
-- pattern until it reaches a stable state with respect to the Hopfield network
-- (represented by @weights@).
-- Pre: @length weights == length pattern@
repeatedUpdate :: (MonadRandom m) => Weights -> Pattern -> m Pattern
repeatedUpdate = checkWsPat repeatedUpdate_


-- TODO Mihaela what is "computeH"? Docs please
computeH :: Weights -> Pattern -> Int -> Int
computeH ws pat i = checkWsPat (\w p -> computeH_ w p i) ws pat


-- | @energy weights pattern@: Computes the energy of a pattern given a Hopfield
-- network (represented by @weights@).
-- Pre: @length weights == length pattern@
energy :: Weights -> Pattern -> Double
energy = checkWsPat energy_



-- | @buildHopfieldData patterns@: Takes a list of patterns and
-- builds a Hopfield network (by training) in which these patterns are
-- stable states. The result of this function can be used to run a pattern
-- against the network, by using 'matchPattern'.
buildHopfieldData :: LearningType -> [Pattern] -> HopfieldData
buildHopfieldData _ []   = error "Train patterns are empty"
buildHopfieldData learningType pats
  | first_len == 0
      = error "Cannot have empty patterns"
  | any (\x -> V.length x /= first_len) pats
      = error "All training patterns must have the same length"
  | otherwise
      = HopfieldData (trainingFunction pats) pats
  where
    first_len = V.length (head pats)
    trainingFunction = case learningType of
                          Hebbian -> train
                          Storkey -> trainStorkey


-- | @train patterns@: Trains and constructs network given a list of patterns
-- which are used to build the weight matrix. As a consequence, they will be
-- stable points in the network (by construction).
train :: [Pattern] -> Weights
train pats = vector2D ws
  -- No need to check pats ws size, buildHopfieldData does it
  where
    ws = [ [ w i j ./. n | j <- [0 .. n-1] ] | i <- [0 .. n-1] ]
    w i j
      | i == j    = 0
      | otherwise = sum [ (pat ! i) * (pat ! j) | pat <- pats ]
    n = V.length (head pats)


-- | See `getUpdatables`.
getUpdatables_ :: Weights -> Pattern -> [(Int, Int)]
getUpdatables_ ws pat = [ (i, new) | (i, x_i) <- zip [0..] (V.toList pat)
                                   , let new = computeH_ ws pat i
                                   , new /= x_i ]


-- | See `computeH`.
computeH_ :: Weights -> Pattern -> Int -> Int
computeH_ ws pat i = if weighted >= 0 then 1 else -1
  where
    weighted = sum [ (ws ! i ! j) *. (pat ! j) | j <- [0 .. p-1] ]
    p = V.length pat


-- | See `updateViaIndex`.
updateViaIndex_ :: [(Int, Int)] -> Int -> Pattern -> Pattern
updateViaIndex_ updatables index pat =
  case updatables of
    [] -> pat
    _  -> V.modify (\v -> write v index (fromJust $ lookup index updatables)) pat


-- | Same as 'update', without size/dimension check, for performance.
update_ :: MonadRandom m => Weights -> Pattern -> m Pattern
update_ ws pat = do
  index <- if null updatables then randomElem [-1] else randomElem $ map fst updatables
  return $ updateViaIndex_ updatables index pat
  where
    updatables = getUpdatables_ ws pat


-- | See `repeatedUpdate`.
repeatedUpdate_ :: (MonadRandom m) => Weights -> Pattern -> m Pattern
repeatedUpdate_ ws pat = repeatUntilEqual (update_ ws) pat


-- | @matchPatterns hopfieldData pattern@:
-- Computes the stable state of a pattern given a Hopfield network(represented
-- by @weights@) and tries to find a match in a list of patterns which are
-- stored in @hopfieldData@.
-- Returns:
--
--    The index of the matching pattern in @patterns@, if a match exists
--    The converged pattern (the stable state), otherwise
--
-- Pre: @length weights == length pattern@
matchPattern :: MonadRandom m => HopfieldData -> Pattern -> m (Either Pattern Int)
matchPattern (HopfieldData ws pats) pat = do
  converged_pattern <- repeatedUpdate_ ws pat
  return $ findInList pats converged_pattern


-- | See `energy`.
energy_ :: Weights -> Pattern -> Double
energy_ ws pat = s / (-2.0)
  where
    p     = V.length pat
    w i j = ws ! i ! j
    x i   = pat ! i
    s     = sum [ w i j *. (x i * x j) | i <- [0 .. p-1], j <- [0 .. p-1] ]


-- | Checks if a pattern consists of only 1s and -1s.
-- Returns @Nothing@ on success, an error string on failure.
validPattern :: Pattern -> Maybe String
validPattern pat = case [ x | x <- V.toList pat, not (x == 1 || x == -1) ] of
  []  -> Nothing
  x:_ -> Just $ "Pattern contains invalid value " ++ show x


-- | @validWeightsPatternSize weights pattern@
-- Returns an error string in a Just if the @pattern@ is not compatible
-- with @weights@ and Nothing otherwise.
validWeightsPatternSize :: Weights -> Pattern -> Maybe String
validWeightsPatternSize ws pat
  | V.length ws /= V.length pat = Just "Pattern size must match network size"
  | otherwise                   = Nothing


-- Checks the validity of a weight matrix by ensuring:
-- * It is non-empty
--
-- * It is square
--
-- * It is symmetric
--
-- * All diagonal elements must be zero
validWeights :: Weights -> Maybe String
validWeights ws
  | n == 0
    = Just "Weight matrix must be non-empty"
  | any (\x -> V.length x /= n) $ V.toList ws
    = Just "Weight matrix has to be a square matrix"
  -- | any (/= 0) [ ws ! i ! i | i <- [0..n-1] ]
  --  = Just "Weight matrix first diagonal must be zero"
  -- | not $ and [ (ws ! i ! j) == (ws ! j ! i) | i <- [0..n-1], j <- [0..n-1] ]
  --  = Just "Weight matrix must be symmetric"
  | null [ abs (ws ! i ! j) > 1 | i <- [0..n-1], j <- [0..n-1] ]
      = Just "Weights should be between (-1, 1)"
  | otherwise = Nothing
  where
    n = V.length ws


-- http://homepages.inf.ed.ac.uk/amos/publications/Storkey1997IncreasingtheCapacityoftheHopfieldNetworkwithoutSacrificingFunctionality.pdf
storkeyHiddenSum :: Weights -> Pattern -> Int -> Int -> Double
storkeyHiddenSum ws pat i j
  = sum [ ws ! i ! k  *. (pat ! k) | k <- [0 .. n - 1] , k /= i , k /= j]
    where n = V.length ws

updateWeightsGivenIndicesStorkey :: Weights -> Pattern -> Int -> Int -> Double
updateWeightsGivenIndicesStorkey ws pat i j
  = ws ! i ! j + 1 ./. n * ( fromIntegral (pat ! i * (pat ! j)) - h j i *. (pat ! i) - h i j *. (pat ! j))
    where n = V.length ws
          h = storkeyHiddenSum ws pat

updateWeightsMatrixStorkey :: Weights -> Pattern -> Weights
updateWeightsMatrixStorkey ws pat
  = vector2D [ [ updateWeightsGivenIndicesStorkey ws pat i j | j <- [0 ..n - 1] ] | i <- [0 ..n - 1] ]
    where n = V.length ws

trainStorkey :: [Pattern] -> Weights
-- No need to check pats ws size, buildHopfieldData does it
trainStorkey pats = foldl updateWeightsMatrixStorkey start_ws pats
    where start_ws = vector2D $ replicate n $ replicate n 0
          n = V.length $ head pats

