{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module BolzmannMachine where
-- | Base Restricted Bolzamann machine.

-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine

-- Using RBM for recognition
-- http://uai.sis.pitt.edu/papers/11/p463-louradour.pdf
-- http://www.dmi.usherb.ca/~larocheh/publications/drbm-mitacs-poster.pdf

import           Data.Maybe
import           Data.Tuple
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.List
import           Data.Vector ((!))
import qualified Data.Vector as V
import qualified Numeric.Container as NC

import Common
import Util

-- In the case of the Bolzamann Machine the weight matrix establishes the
-- weigths between visible and hidden neurons
-- w i j - connection between visible neuron i and hidden neuron j

-- | determines the rate in which the weights are changed in the training phase.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
learningRate = 0.1 :: Double


 data Mode = Hidden | Visible
  deriving(Eq, Show)


 -- data Phase = Training | Matching
--  deriving(Eq, Show)


data BolzmannData = BolzmannData {
    weightsB :: Weights    -- ^ the weights of the network
  , classificationWeights :: Weights -- weigths for classification
  ,  b  :: Pattern -- biases
  ,  c  :: Pattern -- biases
  ,  d  :: Pattern -- biases
  , patternsB :: [Pattern] -- ^ the patterns which were used to train it
  -- can be decuded from weights, maybe should be remove now
  , nr_hidden :: Int       -- ^ number of neurons in the hidden layer
  , nr_classes :: Int
  , pattern_to_class :: [(Pattern, Int)] -- the class of the given pattern
}
  deriving(Show)


-- | Retrieves the dimension of the weights matrix corresponding to the given mode.
-- For hidden, it is the width of the matrix, and for visible it is the height.
getDimension :: Mode -> Weights -> Int
getDimension Hidden ws  = V.length $ ws ! 0
getDimension Visible ws = V.length $ ws


buildBolzmannData ::  MonadRandom  m => [Pattern] ->  m BolzmannData
buildBolzmannData []   = error "Train patterns are empty"
buildBolzmannData pats =
  --nr_hidden <- getRandomR (floor (1.0/ 10.0 * nr_visible), floor (1.0/ 9.0 * nr_visible))
  -- TODO replace with getRandomR with bigger range
  buildBolzmannData' pats (floor (logBase 2 nr_visible) - 3)
    where nr_visible = fromIntegral $ V.length (head pats)


-- | @buildBolzmannData' patterns nr_hidden@: Takes a list of patterns and
-- builds a Bolzmann network (by training) in which these patterns are
-- stable states. The result of this function can be used to run a pattern
-- against the network, by using 'matchPatternBolzmann'.
buildBolzmannData' :: MonadRandom  m => [Pattern] -> Int ->  m BolzmannData
buildBolzmannData' [] _  = error "Train patterns are empty"
buildBolzmannData' pats nr_hidden
  | first_len == 0
      = error "Cannot have empty patterns"
  | any (\x -> V.length x /= first_len) pats
      = error "All training patterns must have the same length"
  | otherwise = trainBolzmann pats nr_hidden
  where
    first_len = V.length (head pats)


gibbsSampling :: Double -> Double -> Int
gibbsSampling r a = if (r < a) then 1 else 0

-- @getActivationProbability ws bias pat index@
-- can be used to compute the activation probability for a neuron in the
-- visible layer, or for parts of the sums requires for
-- the probabilty of the classifications
getActivationSum :: Weights -> Bias -> Pattern -> Int -> Double
getActivationSum ws bias pat index
-- TODO replace with dot product function by using column function for ws
  = bias ! index + sum $ [(ws ! i ! index) *. (pat ! i) | i <- [0 .. p-1] ]
    where
      p = V.length pat


getActivationProbabilityVisible :: Weights -> Bias -> Pattern -> Int -> Double
getActivationProbabilityVisible ws bias h index  = if a <=1 && a >=0 then a else error (show a)
  where a = activation $ getActivationSum ws bias h index


-- assertion same size and move to Util
dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct xs ys = sum [ xs ! i * (ys ! i ) | i <- [0.. V.length xs - 1]]

getActivationSumHidden :: Weights -> Weights ->  Bias -> Pattern -> Pattern -> Int -> Double
getActivationSumHidden ws u c v y index
  = c ! index + dotProduct (ws ! index) (to_double v) + dotProduct (u ! index) (to_double y)
      where to_double = fmap fromIntegral


getHiddenSums :: :: Weights -> Weights ->  Bias -> Pattern -> Pattern -> Int -> Vector Double
getHiddenSums ws u c v y index = V. fromList [getActivationSumHidden ws u c v y i | i <- [0 .. V.length ws ! 0 - 1]]


getActivationProbabilityHidden ::  Weights -> Weights ->  Bias -> Pattern -> Pattern -> Int -> Double
getActivationProbabilityHidden ws u c v y index
  = activation (getActivationSumHidden ws u c v y index)


-- | @updateNeuron mode ws pat index@ , given a vector @pat@ of type @mode@
-- updates the neuron with number @index@ in the layer with opposite type.
updateNeuronVisible :: MonadRandom m => Weights -> Bias -> Pattern -> Int -> m Int
updateNeuron phase ws bias h index = do
  r <- getRandomR (0.0, 1.0)
  return $ gibbsSampling r (getActivationProbabilityVisible ws bias h index)

-- see if one can use map or list comp
updateNeuronHidden :: MonadRandom m => Weights -> Weights ->  Bias -> Pattern -> Pattern -> Int -> m Int
updateNeuronHidden ws u c v y index = do
  r <- getRandomR (0.0, 1.0)
  return $ gibbsSampling r (getActivationProbabilityHidden ws u c v y index)



-- | @getCounterPattern mode ws pat@, given a vector @pat@ of type @mode@
-- computes the values of all the neurons in the layer of the opposite type.
updateVisible :: MonadRandom m => Weights -> Bias -> Pattern -> m Pattern
updateVisible ws bias h
  -- | Just e <- validPattern phase mode ws pat = error e
  -- | otherwise
  = V.fromList `liftM` mapM (updateNeuronVisible ws bias h) updatedIndices
    where
      updatedIndices = [0 .. V.length ws - 1]


updateHidden ::  MonadRandom m => Weights -> Weights -> Weights -> Bias -> Pattern -> Pattern -> m Int
updateHidden ws u c v y = do
  = V.fromList `liftM` mapM (updateNeuronHidden ws u c v y) updatedIndices
    where
      updatedIndices = [0 .. V.length $ ws ! 0 - 1 ]


-- todo add a validClassification function which checks that there is only one
-- 1 in the class pattern
-- TODO replace with actual sampling using inverse method (with cdf list)
updateClassification :: Weights -> Bias -> Pattern -> Pattern
updateClassification u d h = V.fromList [ if n == new_class then 1 else 0 | n <- all_classes]
  where
    new_class   = maximumBy (exp . (getActivationSum u d h) ) all_classes
    all_classes = [0 .. nr_classes - 1]
    nr_classes  = V.length y


-- TODO remove code duplication between this and above
getClassificationVector :: Pattern -> [(Pattern, Int)] -> Int -> Pattern
getClassificationVector pat pat_classes nr_classes =
   = V.fromList [ if n == y_class then 1 else 0 | n <- all_classes]
        where all_classes = [0 .. nr_classes - 1]
              pat_class = fromJust $ lookup pat pat_classes

-- | One step which updates the weights in the CD-n training process.
-- The weights are changed according to one of the training patterns.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
--@oneTrainingStep bm visible class@
oneTrainingStep :: MonadRandom m => BoltzmannData -> Pattern ->  m BoltzmannData
oneTrainingStep (BolzmannData ws u b c d pats nr_h nr_c pat_to_class) v = do
  let y = getClassificationVector v pat_to_class nr_c
  h_sum    <- getHiddenSums ws u c v y
  h        <- updateHidden  ws u c v y
  let y' = updateClassification u d h
  v'       <- updateVisible ws b h
  h_sum'   <- getHiddenSums ws u c v' y'
  let f    = fromDataVector . fmap fromIntegral
      pos_ws  = NC.toLists $ (f v)  `NC.outer` (fromDataVector h_sum)   -- "positive gradient"
      neg_ws  = NC.toLists $ (f v') `NC.outer` (fromDataVector h_sum')  -- "negative gradient"
      pos_u   = NC.toLists $ (f u)  `NC.outer` (fromDataVector h_sum)   -- "positive gradient"
      neg_u   = NC.toLists $ (f u') `NC.outer` (fromDataVector h_sum')  -- "negative gradient"
      d_ws = map (map (* learningRate)) $ combine (-) pos_ws neg_ws     -- weights delta
      new_ws = combine (+) (list2D ws) d_ws
      d_u = map (map (* learningRate)) $ combine (-) pos_u neg_u        -- weights delta
      new_u = combine (+) (list2D ws) d_u
      new_b       = b + learningRate * (v - v')
      new_c       = c + learningRate * (h_sum - h_sum')
      new_d       = d + learningRate * (y - y')
  return $ BoltzmannData (new_ws new_u new_b new_c new_d pats nr_h nr_c pat_to_class)


-- | The training function for the Bolzmann Machine.
-- We are using the contrastive divergence algorithm CD-1
-- TODO see if making the vis
-- (we could extend to CD-n, but "In pratice,  CD-1 has been shown to work surprisingly well."
-- @trainBolzmann pats nr_hidden@ where @pats@ are the training patterns
-- and @nr_hidden@ is the number of neurons to be created in the hidden layer.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
trainBolzmann :: MonadRandom m => [Pattern] -> Int -> m BolzmannData
trainBolzmann pats nr_h = do
  ws <- genWeights
  u  <- genU
  foldM oneTrainingStep (BolzmannData ws u b c d pats nr_h pats_classes nr_classes) pats
    where
      genWeights = replicateM nr_visible . replicateM nr_h $ normal 0.0 0.01
      genU       = replicateM nr_classes . replicateM nr_h $ normal 0.0 0.01
      b  = V.fromList $ replicate nr_visible 0
      c  = V.fromList $ replicate nr_classes 0
      d  = V.fromList $ replicate nr_h 0
      nub_pats = nub pats
      nr_classes = length nub_pats
      pats_classes = [ p_class | p <- zip nub_pats [0 .. ] ]


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


