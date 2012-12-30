{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module ClassificationBoltzmannMachine where
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

import Debug.Trace

-- In the case of the Bolzamann Machine the weight matrix establishes the
-- weigths between visible and hidden neurons
-- w i j - connection between visible neuron i and hidden neuron j

-- | determines the rate in which the weights are changed in the training phase.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
learningRate = 0.000001:: Double


data Mode = Hidden | Visible | Classification
  deriving(Eq, Show)


data BoltzmannData = BoltzmannData {
    weightsB :: Weights    -- ^ the weights of the network
  , classificationWeights :: Weights -- weigths for classification
  ,  b  :: Bias
  ,  c  :: Bias
  ,  d  :: Bias
  , patternsB :: [Pattern] -- ^ the patterns which were used to train it
  -- can be decuded from weights, maybe should be remove now
  , nr_hidden :: Int       -- ^ number of neurons in the hidden layer
  , pattern_to_class :: [(Pattern, Int)] -- the class of the given pattern
    -- classes have to be in consecutive order, from 0
}
  deriving(Show)


-- | Retrieves the dimension of the weights matrix corresponding to the given mode.
-- For hidden, it is the width of the matrix, and for visible it is the height.
getDimension :: Mode -> Weights -> Int
getDimension Hidden  ws = V.length $ ws
getDimension Visible ws = V.length $ ws ! 0
getDimension Classification ws = V.length $ ws ! 0


buildCBoltzmannData ::  MonadRandom m => [Pattern] ->  m BoltzmannData
buildCBoltzmannData []   = error "Train patterns are empty"
buildCBoltzmannData pats =
  buildCBoltzmannData' pats 30
    where nr_visible = V.length (head pats)


-- | @buildBolzmannData' patterns nr_hidden@: Takes a list of patterns and
-- builds a Bolzmann network (by training) in which these patterns are
-- stable states. The result of this function can be used to run a pattern
-- against the network, by using 'matchPatternBolzmann'.
buildCBoltzmannData' :: MonadRandom  m => [Pattern] -> Int ->  m BoltzmannData
buildCBoltzmannData' [] _  = error "Train patterns are empty"
buildCBoltzmannData' pats nr_hidden
  | first_len == 0
      = error "Cannot have empty patterns"
  | any (\x -> V.length x /= first_len) pats
      = error "All training patterns must have the same length"
  | otherwise = trainBolzmann pats nr_hidden
  where
    first_len = V.length $ head pats


-- | @gibbsSampling a@ Gives the binary value of a neuron (0 or 1) from the
-- activation sum
gibbsSampling :: MonadRandom  m => Double -> m Int
gibbsSampling a
  | (a < 0.0 || a > 1.0) = error "argument of gibbsSampling is not a probability"
  | otherwise = do
      r <- getRandomR (0.0, 1.0)
      return $ if (r < a) then 1 else 0


-- | @getActivationProbability ws bias pat index@
-- can be used to compute the activation probability for a neuron in the
-- visible layer, or for parts of the sums requires for
-- the probabilty of the classifications
getActivationSum :: Weights -> Bias -> Pattern -> Int -> Double
getActivationSum ws bias pat index
  = bias ! index + dotProduct (columnVector ws index) (toDouble pat)


-- | @getActivationProbabilityVisible ws bias h index@ returns the activation
-- probability for a neuron @index@ in a visible pattern, given the weights
-- matrix @ws@, the vector of biases @bias@. Applies the activation function
-- to the activation sum, in order to obtain the probability.
getActivationProbabilityVisible :: Weights -> Bias -> Pattern -> Int -> Double
getActivationProbabilityVisible ws bias h index
  = activation $ getActivationSum ws bias h index


-- | @getActivationSumHidden ws bias h index@ returns the activation
-- sum for a neuron @index@ in a hidden pattern, given the weights
-- matrix @ws@, the vector of biases @bias@.
getActivationSumHidden :: Weights -> Weights ->  Bias -> Pattern -> Pattern -> Int -> Double
getActivationSumHidden ws u c v y index
  | Just e <- validPattern Visible ws v = error "Invalid visible pattern in getActivationSumHidden"
  | Just e <- validPattern Classification u y = error "Invalid classification pattern in getActivationSumHidden"
  | otherwise = c ! index + dotProduct (ws ! index) (toDouble v) + dotProduct (u ! index) (toDouble y)

-- | @getActivationSumHidden ws bias h index@ returns the activation
-- sum for all neurons in the hidden pattern, given the weights
-- matrix @ws@, the vector of biases @bias@.
getHiddenSums :: Weights -> Weights ->  Bias -> Pattern -> Pattern -> V.Vector Double
getHiddenSums ws u c v y
  = V.fromList [getActivationSumHidden ws u c v y i | i <- [0 .. (V.length ws) - 1] ]


-- | @getActivationProbabilityVisible ws u bias v index@ returns the activation
-- probability for a neuron @index@ in a hidden pattern, given the weights
-- matrices @ws@ and @u@, the vector of biases @bias@. Applies the activation function
-- to the activation sum, in order to obtain the probability.
getActivationProbabilityHidden ::  Weights -> Weights ->  Bias -> Pattern -> Pattern -> Int -> Double
getActivationProbabilityHidden ws u c v y index
  = activation $ getActivationSumHidden ws u c v y index


-- | @updateNeuronVisible ws bias h index@ updates a neuron in the visible layer by using gibbsSampling, according
-- to the activation probability
updateNeuronVisible :: MonadRandom m => Weights -> Bias -> Pattern -> Int -> m Int
updateNeuronVisible ws bias h index
  = gibbsSampling $ getActivationProbabilityVisible ws bias h index


-- | Updates a neuron in the hidden layer by using gibbsSampling, according
-- to the activation probability
updateNeuronHidden :: MonadRandom m => Weights -> Weights ->  Bias -> Pattern -> Pattern -> Int -> m Int
updateNeuronHidden ws u c v y index
  = gibbsSampling $ getActivationProbabilityHidden ws u c v y index


-- | Updates the entire visible layer by using gibbsSampling, according
-- to the activation probability
updateVisible :: MonadRandom m => Weights -> Bias -> Pattern -> m Pattern
updateVisible ws bias h
   | Just e <- validPattern Hidden ws h = error e
   | otherwise = V.fromList `liftM` mapM (updateNeuronVisible ws bias h) updatedIndices
    where
      updatedIndices = [0 .. (V.length $ ws ! 0) - 1]


-- | Updates the entire visible layer by using gibbsSampling, according
-- to the activation probability
updateHidden ::  MonadRandom m => Weights -> Weights -> Bias -> Pattern -> Pattern -> m Pattern
updateHidden ws u c v y
   | Just e <- validPattern Visible ws v = error e
   | otherwise = V.fromList `liftM` mapM (updateNeuronHidden ws u c v y) updatedIndices
    where
      updatedIndices = [0 .. (V.length ws)  - 1 ]


-- | Updates a classification vector given the current state of the network (
-- the u matrix and the vector of biases d, together with a hidden vector h)
updateClassification :: Weights -> Bias -> Pattern -> Pattern
updateClassification u d h
  = V.fromList [ if n == newClass then 1 else 0 | n <- allClasses]
    where
      -- TODO replace with actual sampling using inverse method (with cdf list)
      expActivation = exp . (getActivationSum u d h)
      newClass   = maximumBy (compareBy expActivation) allClasses
      allClasses = [0 .. nrClasses - 1]
      nrClasses  = V.length d


-- @getClassificationVector pat_to_classes pat@ returns the classification
-- vector of @pat@, by looking up in @pat@ in @pat_to_classes@ to obtain the
-- class of the pattern. The classification vector is obtained by
-- creating vector with all 0s and only 1 in the position of the class.
-- The length of all classification vectors is the number of classes.
getClassificationVector :: [(Pattern, Int)] -> Pattern -> Pattern
getClassificationVector pat_classes pat
  = V.fromList [ if n == pat_class then 1 else 0 | n <- map snd pat_classes]
       where pat_class = fromJust $ lookup pat pat_classes


-- | One step which updates the weights in the CD-n training process.
-- The weights are changed according to one of the training patterns.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
-- @oneTrainingStep bm visible@ updates the parametrs of @bm@ (the 2 weight
-- matrices and the biases) according to the training instance @v@
-- and its classification, obtained by looking in the map kept in @bm@
oneTrainingStep :: MonadRandom m => BoltzmannData -> Pattern ->  m BoltzmannData
oneTrainingStep (BoltzmannData ws u b c d pats nr_h pat_to_class) v = do
  let y     = getClassificationVector pat_to_class v
      h_sum = getHiddenSums ws u c v y
  h        <- updateHidden  ws u c v y
  v'       <- updateVisible ws b h
  let y'   = updateClassification u d h
      (h_sum' :: V.Vector Double) = getHiddenSums ws u c v' y'
      getOuterProduct x y = NC.toLists $ (fromDataVector x)  `NC.outer` (fromDataVector $ toDouble y)
      getDelta pos neg = map (map (* learningRate)) $ combine (-) pos neg
      updateWeights w d_w = vector2D $ combine (+) (list2D w) d_w
      deltaBias v1 v2 = V.map ((* learningRate) . fromIntegral) (combineVectors (-) v1 v2)
      deltaBiasC v1 v2 = V.map (* learningRate) (combineVectors (-) v1 v2)
      updateBias bias delta_bias = combineVectors (+) bias delta_bias
      pos_ws  = getOuterProduct h_sum  v  -- "positive gradient for ws"
      neg_ws  = getOuterProduct h_sum' v' -- "negative gradient for ws"
      pos_u   = getOuterProduct h_sum  y  -- "positive gradient for u"
      neg_u   = getOuterProduct h_sum' y' -- "negative gradient for u"
      d_ws    = getDelta pos_ws neg_ws    -- "delta ws"
      new_ws  = updateWeights ws d_ws
      d_u     = getDelta pos_u neg_u      -- "delta u"
      new_u   = updateWeights u d_u
      new_b   = updateBias b (deltaBias v v')
      new_c   = updateBias c (deltaBiasC h_sum h_sum')
      new_d   = updateBias d (deltaBias y y')
  return $ BoltzmannData new_ws new_u new_b new_c new_d pats nr_h pat_to_class


-- | The training function for the Bolzmann Machine.
-- We are using the contrastive divergence algorithm CD-1
-- TODO see if making the vis
-- (we could extend to CD-n, but "In pratice,  CD-1 has been shown to work surprisingly well."
-- @trainBolzmann pats nr_hidden@ where @pats@ are the training patterns
-- and @nr_hidden@ is the number of neurons to be created in the hidden layer.
-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine#Training_algorithm
trainBolzmann :: MonadRandom m => [Pattern] -> Int -> m BoltzmannData
trainBolzmann pats nr_h = do
  ws <- vector2D `liftM` genWeights
  u  <- vector2D `liftM` genU
  foldM oneTrainingStep (BoltzmannData ws u b c d pats nr_h pats_classes) pats
    where
      genWeights = replicateM nr_h . replicateM nr_visible $ normal 0.0 0.01
      genU       = replicateM nr_h . replicateM nr_classes $ normal 0.0 0.01
      b  = V.fromList $ replicate nr_visible 0.0
      c  = V.fromList $ replicate nr_h 0.0
      d  = V.fromList $ replicate nr_classes 0.0
      nr_classes = length nub_pats
      nub_pats = nub pats
      pats_classes = zip nub_pats [0 .. ]
      nr_visible = V.length $ head pats


-- | @matchPatternBoltzmann bm pat@ given the boltzmann trained network @bm@
-- regonizes @pat@, by classifing it to one of the patterns the network was
-- trained with. This is done by computing the free energy of @pat@ with
-- every possible classification, and choosing the classification with
-- lowest energy.
-- http://uai.sis.pitt.edu/papers/11/p463-louradour.pdf
matchPatternCBoltzmann :: BoltzmannData -> Pattern -> Int
matchPatternCBoltzmann bm v
  = trace (show $ map (probability . snd) patternsWithClassifications) fromJust $ maxPat `elemIndex` pats
    where
      pats_classes = pattern_to_class bm
      pats = patternsB bm
      patternsWithClassifications = [ (p, getClassificationVector pats_classes p) | p <- map fst pats_classes]
      probability classification = exp $ - (getFreeEnergy bm v classification)
      (maxPat, _) = maximumBy (compareBy $ probability . snd) patternsWithClassifications


-- | @getFreeEnergy bm visible classification_vector@
-- Computes the free energy of @v@ with @classification_vector@, according
-- to the trained boltzmann network @bm@. It is used for classifing a given
-- visible vector according to the classes used for training the network @bm@.
getFreeEnergy :: BoltzmannData -> Pattern -> Pattern -> Double
getFreeEnergy (BoltzmannData ws u b c d pats nr_h pats_classes) v y
  = - dotProduct d (toDouble y) - sum [ f i | i <- [0 .. nr_h - 1] ]
      where f = softplus . (getActivationSumHidden ws u c v y)


-- | The activation functiom for the network (the logistic sigmoid).
-- http://en.wikipedia.org/wiki/Sigmoid_function
activation :: Double -> Double
activation x = 1.0 / (1.0 + exp (-x))

-- | The function used to compute the free energy
-- http://uai.sis.pitt.edu/papers/11/p463-louradour.pdf
softplus :: Double -> Double
softplus x = log (1.0 + exp x)


-- TODO move to tests
validClassificationVector :: Pattern -> Int -> Maybe String
validClassificationVector pat size
  | V.length pat /= size = Just "classification vector does not match expected size"
  | V.any (\x -> notElem x [0, 1]) pat   = Just "Non binary element in classification pattern"
  | V.sum pat /=1 = Just "Invalid classification vector"
  | otherwise = Nothing


-- | @validPattern mode weights pattern@
-- Returns an error string in a Just if the @pattern@ is not compatible
-- with @weights@ and Nothing otherwise. @mode@ gives the type of the pattern,
-- which is checked (Visible or Hidden).
validPattern :: Mode -> Weights -> Pattern -> Maybe String
validPattern mode ws pat
  | getDimension mode ws /= V.length pat = Just $ "Size of pattern must match network size in " ++ show mode
  | V.any (\x -> notElem x [0, 1]) pat   = Just "Non binary element in bolzmann pattern"
  | otherwise                            = Nothing

-- | @validWeights ws@ checks that a weight matrix is well formed.
validWeights :: Weights -> Maybe String
validWeights ws
  | V.null ws = Just "The  matrix of weights is empty"
  | V.any (\x -> V.length x /= V.length (ws ! 0)) ws = Just "Weigths matrix ill formed"
  | otherwise = Nothing

