{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Base Hopfield model, providing training and running.
module Hopfield.Hopfield (
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
  , addPatterns
  , repeatedUpdate
  , updateChain
  , matchPattern
  , computeH
  -- * Energy
  , energy
) where

import           Control.Monad
import           Control.Monad.Random (MonadRandom)
import           Data.Maybe
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Data.Vector.Generic.Mutable (write)

import           Hopfield.Common
import           Hopfield.Util



data LearningType = Hebbian | Storkey deriving (Eq, Show, Read)

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
update :: MonadRandom m => Weights -> Pattern -> m (Maybe Pattern)
update = checkWsPat update_


-- | @repeatedUpdate weights pattern@: Performs repeated updates on the given
-- pattern until it reaches a stable state with respect to the Hopfield network
-- (represented by @weights@).
-- Pre: @length weights == length pattern@
repeatedUpdate :: (MonadRandom m) => Weights -> Pattern -> m Pattern
repeatedUpdate = checkWsPat repeatedUpdate_


-- | Computes the weighted sum of current neuron values, which will give us
-- the value of the neuron (by taking the sign)
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


-- | See `computeH`.
computeH_ :: Weights -> Pattern -> Int -> Int
computeH_ ws pat i = {-# SCC "computeHall" #-} if weighted >= 0 then 1 else -1
  where
    weighted :: Double
    wss = ws ! i
    weighted = go 0 0.0
    go :: Int -> Double -> Double
    go !j !s | j == p    = s
             | otherwise = let w  = wss `V.unsafeIndex` j
                               x = if pat `V.unsafeIndex` j > 0 then w
                                                                 else -w
                            in go (j+1) (s+x)

    p = {-# SCC "computeHvlength" #-} V.length pat



-- | See `update`.
-- The update is done by finding a neuron that will change its value given the
-- current state. The search for this neuron is done in a random manner:
-- pick up a random neuron, check if it is updatable: if so, update the pattern
-- by updating this neuron. If not, continue until an updatable neuron is found.
-- (Note: Initially the update was performed by obtaining a list of all
-- updatable neurons and then picking a random one. The current method is 2 times
-- faster)
update_ :: MonadRandom m => Weights -> Pattern -> m (Maybe Pattern)
update_ ws pat = do
  randomIndices <- shuffle . toArray $ [0 .. V.length pat - 1]
  -- TODO avoid Array -> List -> Vector conversion
  return $ case firstUpdatable (V.fromList randomIndices) of
    Nothing -> Nothing
    Just index -> Just $ flipAtIndex pat index
  where
     firstUpdatable indices = go 0
       where
         go n
           | n == V.length pat             = Nothing
           | pat ! i /= computeH_ ws pat i = Just i
           | otherwise                     = go (n+1)
           where i = indices ! n

     flipAtIndex vec index = let val = vec ! index -- seq only brings small saving here
                              in val `seq` V.modify (\v -> write v index (-val)) vec


-- | See `repeatedUpdate`.
repeatedUpdate_ :: (MonadRandom m) => Weights -> Pattern -> m Pattern
repeatedUpdate_ ws pat = repeatUntilNothing (update_ ws) pat


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


-- | Like `repeatedUpdate`, but collecting all patterns until convergence.
-- The last pattern in the list is the converged pattern.
-- The argument pattern is NOT prepended to the result list.
--
-- POST: The returned list is not empty.
updateChain :: (MonadRandom m) => HopfieldData -> Pattern -> m [Pattern]
updateChain (HopfieldData ws _pats) pat
  | Just e <- validPattern pat = error e
  | otherwise                  = (pat:) `liftM` unfoldrSelfM (update_ ws) pat


-- | Stores patterns in an already trained network. One has to ensure that this
-- function is not over used, as this will decrease the capacity of the network.
addPatterns ::  LearningType -> HopfieldData -> [Pattern] -> HopfieldData
addPatterns learning (HopfieldData ws pats) addedPats
  | any (isJust . validPattern) addedPats = error "invalid patterns in addMultiplePatterns"
  | any (isJust . validWeightsPatternSize ws) addedPats = error "pattern does not match weights in addMultiplePatterns"
  | otherwise = HopfieldData new_ws (pats ++ addedPats)
      where new_ws = foldl (updateWeightsGivenNewPattern learning) ws addedPats


-- Updates the weight matrix when a new pattern is stored in the network
updateWeightsGivenNewPattern :: LearningType -> Weights -> Pattern -> Weights
updateWeightsGivenNewPattern Storkey ws pat = updateWeightsStorkey ws pat
updateWeightsGivenNewPattern Hebbian ws pat = vector2D updated_ws
  where updated_ws = [ [ws ! i ! j + (pat ! i * pat ! j) ./. n | j <- neurons ] | i <- neurons]
        n = V.length ws - 1
        neurons = [0 .. n]


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
-- These checks hold for both Hebbian and Storkey.
validWeights :: Weights -> Maybe String
validWeights ws
  | n == 0
    = Just "Weight matrix must be non-empty"
  | any (\x -> V.length x /= n) $ V.toList ws
    = Just "Weight matrix has to be a square matrix"
  | any (/= 0) [ ws ! i ! i | i <- [0..n-1] ]
    = Just "Weight matrix first diagonal must be zero"
  | not $ and [ abs( (ws ! i ! j) - (ws ! j ! i) ) < 0.0001 | i <- [0..n-1], j <- [0..n-1] ]
     = Just "Weight matrix must be symmetric"
  | null [ abs (ws ! i ! j) > 1 | i <- [0..n-1], j <- [0..n-1] ]
      = Just "Weights should be between (-1, 1)"
  | otherwise = Nothing
  where
    n = V.length ws


-- Storkey training provides advantages for the Hopfield network as
-- it gives it bigger capacity and higher basins of attraction.
-- For more details see:
-- http://homepages.inf.ed.ac.uk/amos/publications/Storkey1997IncreasingtheCapacityoftheHopfieldNetworkwithoutSacrificingFunctionality.pdf


-- | @storkeyHiddenSum  ws pat i j@ computes the value at indices @i@ @j@  in the
-- hidden matrix which is used for updating in the weight matrix during trainig
-- given the training pattern @pat@.
storkeyHiddenSum :: Weights -> Pattern -> Int -> Int -> Double
storkeyHiddenSum ws pat i j
    = sum [ ws ! i ! k  *. (pat ! k) | k <- [0 .. n - 1] , k /= i , k /= j]
      where n = V.length ws

-- | @updateWeightsGivenIndicesStorkey ws pat i j@ computes the new value at
-- indices @i@ @j@  of the weights matrix for the training iteration of
-- pattern @pat@.
updateWeightsGivenIndicesStorkey :: Weights -> Pattern -> Int -> Int -> Double
updateWeightsGivenIndicesStorkey ws pat i j
  | i == j = 0.0
  | otherwise = ws ! i ! j + (1 :: Int) ./. n * (fromIntegral (pat ! i * (pat ! j)) - h j i *. (pat ! i) - h i j *. (pat ! j))
  where n = V.length ws
        h = storkeyHiddenSum ws pat


-- | @updateWeightsStorkey ws pat@ updates the weights matrix, given training
-- instance @pat@.
updateWeightsStorkey :: Weights -> Pattern -> Weights
updateWeightsStorkey ws pat
  = vector2D [ [ updateWeightsGivenIndicesStorkey ws pat i j | j <- [0 ..n - 1] ] | i <- [0 ..n - 1] ]
    where n = V.length ws


-- | @trainStorkey pats@ trains the Hopfield network by computing the weights
-- matrix by iterating trough all training instances (@pats@) and updating the
-- weights according to the Storkey learning rule.
trainStorkey :: [Pattern] -> Weights
-- No need to check pats ws size, buildHopfieldData does it
trainStorkey pats = foldl updateWeightsStorkey start_ws pats
    where start_ws = vector2D $ replicate n $ replicate n 0
          n = V.length $ head pats

