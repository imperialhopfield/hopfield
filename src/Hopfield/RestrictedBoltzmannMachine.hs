{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

-- | Base Restricted Bolzamann machine.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine
module Hopfield.RestrictedBoltzmannMachine where


import           Data.Maybe
import           Control.Monad
import           Control.Monad.Random
import           Data.List
import           Data.Vector ((!))
import qualified Data.Vector as V
import qualified Numeric.Container as NC

import Hopfield.Common
import Hopfield.Util


-- In the case of the Bolzamann Machine the weight matrix establishes the
-- weigths between visible and hidden neurons
-- w i j - connection between visible neuron i and hidden neuron j

-- | determines the rate in which the weights are changed in the training phase.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
learningRate :: Double
learningRate = 0.1


data Mode = Hidden | Visible
 deriving(Eq, Show)


data Phase = Training | Matching
 deriving(Eq, Show)


data BoltzmannData = BoltzmannData {
    weightsB :: Weights    -- ^ the weights of the network
  , patternsB :: [Pattern] -- ^ the patterns which were used to train it
  , nr_hiddenB :: Int      -- ^ number of neurons in the hidden layer
  , pattern_to_binaryB :: [(Pattern, [Int])] -- ^ the binary representation of the pattern index
      -- the pattern_to_binary field will not replace the patternsB field as it does
      -- not contain duplicated patterns, which might be required for statistical
      -- analysis in clustering and super attractors
}
  deriving(Show)


-- | Retrieves the dimension of the weights matrix corresponding to the given mode.
-- For hidden, it is the width of the matrix, and for visible it is the height.
getDimension :: Mode -> Weights -> Int
getDimension Hidden ws  = V.length $ ws ! 0
getDimension Visible ws = V.length $ ws


notMode :: Mode -> Mode
notMode Visible = Hidden
notMode Hidden  = Visible


buildBoltzmannData ::  MonadRandom  m => [Pattern] ->  m BoltzmannData
buildBoltzmannData []   = error "Train patterns are empty"
buildBoltzmannData pats =
  buildBoltzmannData' pats nr_visible
    where nr_visible = fromIntegral $ V.length (head pats)


-- | @buildBoltzmannData' patterns nr_hidden@: Takes a list of patterns and
-- builds a Bolzmann network (by training) in which these patterns are
-- stable states. The result of this function can be used to run a pattern
-- against the network, by using 'matchPatternBolzmann'.
buildBoltzmannData' :: MonadRandom  m => [Pattern] -> Int ->  m BoltzmannData
buildBoltzmannData' [] _  = error "Train patterns are empty"
buildBoltzmannData' pats nr_hidden
  | first_len == 0
      = error "Cannot have empty patterns"
  | any (\x -> V.length x /= first_len) pats
      = error "All training patterns must have the same length"
  | otherwise = do
      (ws, pats_with_binary) :: (Weights, [(Pattern, [Int])]) <- trainBolzmann pats nr_hidden
      return $ BoltzmannData ws pats nr_hidden pats_with_binary
  where
    first_len = V.length (head pats)


-- Pure version of updateNeuron for testing
updateNeuron' ::  Double -> Phase -> Mode -> Weights -> Pattern -> Int -> Int
updateNeuron' r phase mode ws pat index = if (r < a) then 1 else 0
  where a = getActivationProbability phase mode ws pat index


getActivationProbability :: Phase -> Mode -> Weights -> Pattern -> Int -> Double
getActivationProbability phase mode ws pat index = if a <=1 && a >=0 then a else error (show a)
  where
    a = activation . sum $ case mode of
      Hidden   -> [ (ws ! index ! i) *. (pat' ! i) | i <- [0 .. p-1] ]
      Visible  -> [ (ws ! i ! index) *. (pat' ! i) | i <- [0 .. p-1] ]
    pat' = case phase of
            Matching -> V.cons 1 pat
            Training -> pat
    p = V.length pat'


-- | @updateNeuron mode ws pat index@ , given a vector @pat@ of type @mode@
-- updates the neuron with number @index@ in the layer with opposite type.
updateNeuron :: MonadRandom m => Phase -> Mode -> Weights -> Pattern -> Int -> m Int
updateNeuron phase mode ws pat index = do
  r <- getRandomR (0.0, 1.0)
  return $ updateNeuron' r phase mode ws pat index


-- | @getCounterPattern mode ws pat@, given a vector @pat@ of type @mode@
-- computes the values of all the neurons in the layer of the opposite type.
getCounterPattern :: MonadRandom m => Phase -> Mode -> Weights -> Pattern -> m Pattern
getCounterPattern phase mode ws pat
  | Just e <- validPattern phase mode ws pat = error e
  | otherwise = V.fromList `liftM` mapM (updateNeuron phase mode ws pat) updatedIndices
    where
      updatedIndices = [0 .. getDimension (notMode mode) ws - diff]
      diff = case phase of
              Training -> 1
              Matching -> 2


-- | One step which updates the weights in the CD-n training process.
-- The weights are changed according to one of the training patterns.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
updateWeights :: MonadRandom m => Weights -> Pattern -> m Weights
updateWeights ws v = do
  let biased_v = V.cons 1 v
  h        <- getCounterPattern Training Visible ws biased_v
  v'       <- getCounterPattern Training Hidden  ws h
  let f    = fromDataVector . fmap fromIntegral
      pos  = NC.toLists $ (f biased_v) `NC.outer` (fromDataVector $ getSigmaH v)   -- "positive gradient"
      neg  = NC.toLists $ (f v') `NC.outer` (fromDataVector $ getSigmaH v') -- "negative gradient"
      d_ws = map (map (* learningRate)) $ combine (-) pos neg -- weights delta
      new_weights = combine (+) (list2D ws) d_ws
      nr_hidden   = V.length $ ws ! 0
      getSigmaH y = V.fromList [getActivationProbability Training Visible ws y x | x <- [0.. nr_hidden - 1] ]
  return $ vector2D new_weights


-- | The training function for the Bolzmann Machine.
-- We are using the contrastive divergence algorithm CD-1
-- TODO see if making the vis
-- (we could extend to CD-n, but "In pratice,  CD-1 has been shown to work surprisingly well."
-- @trainBolzmann pats nr_hidden@ where @pats@ are the training patterns
-- and @nr_hidden@ is the number of neurons to be created in the hidden layer.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
trainBolzmann :: MonadRandom m => [Pattern] -> Int -> m (Weights, [(Pattern, [Int])])
trainBolzmann pats nr_hidden = do
  weights_without_bias <- genWeights
  -- add biases as a dimension of the matrix, in order to include them in the
  -- contrastive divergence algorithm
  let ws = [0: x | x <- weights_without_bias]
      ws_start  = (replicate (nr_hidden + 1) 0) : ws
  updated_ws <- foldM updateWeights (vector2D ws_start) pats'
  return (updated_ws, paths_with_binary_indices)
    where
      genWeights = replicateM nr_visible . replicateM nr_hidden $ normal 0.0 0.01
      paths_with_binary_indices = getBinaryIndices pats
      pats' = [(V.++) x $ encoding x | x <- pats]
      encoding x = V.fromList . fromJust $ lookup x paths_with_binary_indices
      nr_visible = V.length $ pats' !! 0


-- | The activation functiom for the network (the logistic sigmoid).
-- http://en.wikipedia.org/wiki/Sigmoid_function
activation :: Double -> Double
activation x = 1.0 / (1.0 + exp (-x))


-- | @validPattern mode weights pattern@
-- Returns an error string in a Just if the @pattern@ is not compatible
-- with @weights@ and Nothing otherwise. @mode@ gives the type of the pattern,
-- which is checked (Visible or Hidden).
validPattern :: Phase -> Mode -> Weights -> Pattern -> Maybe String
validPattern phase mode ws pat
  | checked_dim /= V.length pat        = Just $ "Size of pattern must match network size in " ++ show phase ++ " " ++ show mode
  | V.any (\x -> notElem x [0, 1]) pat = Just "Non binary element in bolzmann pattern"
  | otherwise            = Nothing
  where checked_dim = if phase == Training then actual_dim else actual_dim - 1
        actual_dim  = getDimension mode ws


validWeights :: Weights -> Maybe String
validWeights ws
  | V.null ws = Just "The  matrix of weights is empty"
  | V.any (\x -> V.length x /= V.length (ws ! 0)) ws = Just "Weigths matrix ill formed"
  | otherwise = Nothing


updateBoltzmann :: MonadRandom m => Weights -> Pattern -> m Pattern
updateBoltzmann ws pat = do
  h <- getCounterPattern Matching Visible ws pat
  getCounterPattern Matching Hidden ws h


-- see http://www.cs.toronto.edu/~hinton/absps/guideTR.pdf section 16.1
-- And stack overflow discussion
-- http://stackoverflow.com/questions/9944568/the-free-energy-approximation-equation-in-restriction-boltzmann-machines
-- http://www.dmi.usherb.ca/~larocheh/publications/class_set_rbms_uai.pdf
getFreeEnergy :: Weights -> Pattern -> Double
getFreeEnergy ws pat
  | Just e <- validWeights ws                      = error e
  | Just e <- validPattern Matching Visible ws pat = error e
  | otherwise = - biases - sum (map f xs)
    where w i j = ((ws :: Weights) ! i ! j) :: Double
          biases = sum [ w (i + 1) 0  *. (pat ! i)        | i <- [0 .. p - 1] ]
          xs = [ w 0 j + sum [ w (i + 1) j *.  (pat ! i)  | i <- [0 .. p - 1] ] | j <- [1 .. (V.length $ ws ! 0) - 1]]
          f x = log (1 + exp x)
          p = V.length pat

-- TODO Mihaela. See if we need to do repeated or just simple update
matchPatternBoltzmann :: MonadRandom m => BoltzmannData -> Pattern -> m Int
matchPatternBoltzmann (BoltzmannData ws pats _ pats_with_binary) pat = do
  hot_pat <- repeatUntilEqualOrLimitExceeded 1000 (updateBoltzmann ws) ((V.++) pat (V.fromList $ snd $ head pats_with_binary))
  let h = V.take (V.length $ head pats) hot_pat
      extendWithClass p = ((V.++) h (V.fromList . fromJust $ lookup p pats_with_binary) )
      getPatternProbability x = exp $ (- getFreeEnergy ws x)
      fromPatToIndex p = fromJust $ p `elemIndex` pats
  return $ fst $ maximumBy (compareBy snd) [(fromPatToIndex p, (getPatternProbability . extendWithClass) p) | p <- pats]

