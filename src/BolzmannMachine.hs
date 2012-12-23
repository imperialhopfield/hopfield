{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module BolzmannMachine where
-- | Base Restricted Bolzamann machine.

-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import qualified Data.Random as DR
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Data.Word (Word32)
import qualified Numeric.Container as NC

import Common
import Util

-- In the case of the Bolzamann Machine the weight matrix establishes the
-- weigths between visible and hidden neurons
-- w i j - connection between visible neuron i and hidden neuron j



-- start with no biases initially, introduce them after (if needed)
-- if biases are used, they should be normally distributed


-- | determines the rate in which the weights are changed in the training phase.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
learningRate = 0.1 :: Double


data Mode = Hidden | Visible


data BolzmannData = BolzmannData {
    weightsB :: Weights    -- ^ the weights of the network
  , patternsB :: [Pattern] -- ^ the patterns which were used to train it
  , nr_hidden :: Int      -- ^ number of neurons in the hidden layer
}
  deriving(Show)

-- | Gives the opposite type of layer.
notMode :: Mode -> Mode
notMode Hidden  = Visible
notMode Visible = Hidden

-- | Retrieves the dimension of the weights matrix corresponding to the given mode.
-- For hidden, it is the width of the matrix, and for visible it is the height.
getDimension :: Mode -> Weights -> Int
getDimension Hidden ws = V.length $ ws ! 0
getDimension Visible ws = V.length $ ws


-- | @buildHopfieldData patterns@: Takes a list of patterns and
-- builds a Hopfield network (by training) in which these patterns are
-- stable states. The result of this function can be used to run a pattern
-- against the network, by using 'matchPattern'.
buildBolzmannData :: MonadRandom  m => [Pattern] -> Int ->  m BolzmannData
buildBolzmannData [] _  = error "Train patterns are empty"
buildBolzmannData pats nr_hidden
  | first_len == 0
      = error "Cannot have empty patterns"
  | any (\x -> V.length x /= first_len) pats
      = error "All training patterns must have the same length"
  | otherwise = do
      ws:: Weights <- trainBolzmann pats nr_hidden
      return $ BolzmannData ws pats nr_hidden
  where
    first_len = V.length (head pats)


-- Pure version of updateNeuron for testing
updateNeuron'::  Double -> Mode -> Weights -> Pattern -> Int -> Int
updateNeuron' r mode ws pat index = if (r < a) then 1 else 0
  where a = getActivationProbability mode ws pat


getActivationProbability :: Mode -> Weights -> Pattern -> Double
getActivationProbability mode ws pat = activation . sum $ case mode of
    Hidden   -> [ (ws ! index ! i) *. (pat ! i) | i <- [0 .. p-1] ]
    Visible  -> [ (ws ! i ! index) *. (pat ! i) | i <- [0 .. p-1] ]
  where p = V.length pat


-- | @updateNeuron mode ws pat index@ , given a vector @pat@ of type @mode@
-- updates the neuron with number @index@ in the layer with opposite type.
updateNeuron :: MonadRandom m => Mode -> Weights -> Pattern -> Int -> m Int
updateNeuron mode ws pat index = do
  r <- getRandomR (0.0, 1.0)
  return $ updateNeuron' r mode ws pat index


-- | @getCounterPattern mode ws pat@, given a vector @pat@ of type @mode@
-- computes the values of all the neurons in the layer of the opposite type.
getCounterPattern :: MonadRandom m => Mode -> Weights -> Pattern -> m Pattern
getCounterPattern mode ws pat
  | Just e <- validPattern mode ws pat = error e
  | otherwise = V.fromList `liftM` mapM (updateNeuron mode ws pat) updatedIndices
    where
      updatedIndices = [0 .. getDimension (notMode mode) ws - 1]


-- | One step which updates the weights in the CD-n training process.
-- The weights are changed according to one of the training patterns.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
updateWeights :: MonadRandom m => Weights -> Pattern -> m Weights
updateWeights ws v = do
  h        <- getCounterPattern Visible ws v
  v'       <- getCounterPattern Hidden  ws h
  h'       <- getCounterPattern Visible ws v'
  let f    = fromDataVector . fmap fromIntegral
      pos  = NC.toLists $ (f v) `NC.outer` (f h)   -- "positive gradient"
      neg  = NC.toLists $ (f v') `NC.outer` (f h') -- "negative gradient"
      d_ws = map (map (* learningRate)) $ combine (-) pos neg -- weights delta
      new_weights = combine (+) (list2D ws) d_ws
  return $ vector2D new_weights


-- | The training function for the Bolzmann Machine.
-- We are using the contrastive divergence algorithm CD-1
-- (we could extend to CD-n).
-- @trainBolzmann pats nr_hidden@ where @pats@ are the training patterns
-- and @nr_hidden@ is the number of neurons to be created in the hidden layer.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
trainBolzmann :: MonadRandom m => [Pattern] -> Int -> m Weights
trainBolzmann pats nr_hidden = do
  ws_start <- genWeights
  foldM updateWeights (vector2D ws_start) pats
    where
      genWeights = replicateM nr_visible . replicateM nr_hidden $ normal 0.0 0.01
      nr_visible = V.length $ pats !! 0


-- | The activation functiom for the network (the logistic sigmoid).
-- http://en.wikipedia.org/wiki/Sigmoid_function
activation :: Double -> Double
activation x = 1.0 / (1.0 - exp (-x))


-- | @validPattern mode weights pattern@
-- Returns an error string in a Just if the @pattern@ is not compatible
-- with @weights@ and Nothing otherwise. @mode@ gives the type of the pattern,
-- which is checked (Visible or Hidden).
validPattern :: Mode -> Weights -> Pattern -> Maybe String
validPattern mode ws pat
  | getDimension mode ws /= V.length pat = Just "Size of pattern must match network size"
  | otherwise            = Nothing


 -- | Generates a number sampled from a random distribution, given the mean and
 -- standard deviation.
normal :: forall m . MonadRandom m => Double -> Double -> m Double
normal m std = do
  r <- DR.runRVar (DR.normal m std) (getRandom :: MonadRandom m => m Word32)
  return r

-- | Does one update of a visible pattern by updating the hidden layer neurons once
-- and then using the new values to obtain new values for the visible layer.
updateBolzmann :: MonadRandom m => Weights -> Pattern -> m Pattern
updateBolzmann ws pat = do
  h <- getCounterPattern Visible ws pat
  getCounterPattern Hidden ws h


-- | Repeates an update until the pattern converges (does not change any
-- more on further updates).
repeatedUpdateBolzmann :: MonadRandom m => Weights -> Pattern -> m Pattern
repeatedUpdateBolzmann ws pat = repeatUntilEqual (updateBolzmann ws) pat


main = do
  gen  <- getStdGen
  let v1 = V.fromList [0, 1, 0]
  let v2 = V.fromList [1, 0, 0]
  let v3 = V.fromList [1, 1, 0]
  let ws = evalRand (trainBolzmann [v1, v2, v3] 4) gen
  return $ evalRand (repeatedUpdateBolzmann ws (V.fromList [1, 0, 1])) gen
