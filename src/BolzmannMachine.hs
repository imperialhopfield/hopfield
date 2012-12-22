{-# LANGUAGE PatternGuards #-}

module BolzmannMachine where
-- | Base Restricted Bolzamann machine.

-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine

import           Control.Monad
import           Control.Monad.Random
import           Data.Vector ((!))
import qualified Data.Vector as V
import qualified Numeric.Container as NC

import Common
import Util

--TODO CHECK IF IT IS OK WITH -1 and 1 instead of 0 and 1

-- In the case of the Bolzamann Machine the weight matrix establishes the
-- weigths between visible and hidden neurons
-- w i j - connection between visible neuron i and hidden neuron j

-- start with no biases initially, introduce them after (if needed)
-- if biases are used, they should be normally distributed


lr = 0.1 :: Double -- learning rate

-- TODO checks for Weigths. Here there are different
-- TODO checks for patterns. Here there are different
-- TODO see order of updates using shuffle
-- TODO see how you can change update and co from Hopfield to fit this

-- checks are done once in getHidden and getVisible, for efficiency

data Mode = Hidden | Visible

updateCounterPattern :: MonadRandom m => Mode -> Weights -> Pattern -> Int -> m Int
updateCounterPattern mode ws pat index = do
  r <- getRandomR (0.0, 1.0)
  return $ if (r < a) then 1 else -1
    where
      a = activation . sum $ case mode of
            Hidden  -> [ (ws ! index ! i) *. (pat ! i) | i <- [0 .. p-1] ]
            Visible -> [ (ws ! i ! index) *. (pat ! i) | i <- [0 .. p-1] ]
      p = V.length pat

getHidden:: MonadRandom m => Weights -> Pattern -> m Pattern
getHidden ws v
  | Just e <- validVisiblePattern ws v  = error e
  | otherwise = do
      h <- mapM (updateHidden ws v) [0.. (V.length $ ws ! 0) - 1]
      return $ V.fromList h


getVisible:: MonadRandom m => Weights -> Pattern -> m Pattern
getVisible ws h
  | Just e <- validHiddenPattern ws h  = error e
  | otherwise = do
      v <- mapM (updateVisible ws h) [0.. (V.length ws) - 1]
      return $ V.fromList v


updateWS:: MonadRandom m => Weights -> Pattern -> m Weights
updateWS ws v = do
  h         <- getHidden ws v
  v'        <- getVisible ws h
  h'        <- getHidden ws v'
  let f     = fromDataVector . fmap fromIntegral
      pos   = NC.toLists $ (f v) `NC.outer` (f h)
      neg   = NC.toLists $ (f v') `NC.outer` (f h')
      dws   = combine (-) pos neg
      ws_f  = combine (+) (list2D ws) dws
      ws_l = map (map (* lr)) ws_f
  return $ vector2D ws_l


-- CD-1 (we could extend to CD-n)
train :: MonadRandom m => [Pattern] -> Int -> m Weights
train pats nr_hidden = do
  ws_start <- ws_start''
  foldM updateWS ws_start pats
    where ws_start'  = take nr_visible (repeat $ take nr_hidden $ repeat $ normal 0.0 0.01)
          ws_start'' = liftM vector2D (sequence $ map sequence ws_start')
          nr_visible = V.length $ pats !! 0

activation :: Double -> Double
activation x = 1.0 / (1.0 - exp (-x))


-- | @validPattern weights pattern@
-- Returns an error string in a Just if the @pattern@ is not compatible
-- with @weights@ and Nothing otherwise.
validHiddenPattern :: Weights -> Pattern -> Maybe String
validHiddenPattern ws h
  | V.length (ws ! 0) /= V.length h = Just "Size of hidden must match network size"
  | otherwise                   = Nothing


-- | @validPattern weights pattern@
-- Returns an error string in a Just if the @pattern@ is not compatible
-- with @weights@ and Nothing otherwise.
validVisiblePattern :: Weights -> Pattern -> Maybe String
validVisiblePattern ws v
  | V.length ws /= V.length v = Just "Size of visible pattern must match network size"
  | otherwise                   = Nothing


-- TODO move to Util

-- |Generates uniform random variables.
unif :: (MonadRandom m) => m Double
unif = getRandomR (0, 1)

-- |Generate two samples from the standard normal distribution, using
--  the Box-Muller method.
stdNormals :: (MonadRandom m) => m (Double,Double)
stdNormals = do
  u <- unif
  v <- unif
  let r = sqrt((-2) * log u)
  let arg1 = cos (2 * pi * v)
  let arg2 = sin (2 * pi * v)
  return (r * arg1, r * arg2)

-- |Generate a single sample from the standard normal distribution, by
--  generating two samples and throwing away the second one.
stdNormal :: (MonadRandom m) => m Double
stdNormal = do
  (x, _) <- stdNormals
  return x

-- |Generate a sample from the standard normal distribution with a given
--  mean and variance.
normal :: (MonadRandom m) => Double -> Double -> m Double
normal mu sigma = do
  x <- stdNormal
  return $ mu + sigma * x

update1 :: MonadRandom m => Weights -> Pattern -> m Pattern
update1 ws pat = do
  h <- getHidden ws pat
  getVisible ws h

repeatedUpdate1 :: MonadRandom m => Weights -> Pattern -> m Pattern
repeatedUpdate1 ws pat = repeatUntilEqual (update1 ws) pat

main = do
  gen  <- getStdGen
  let v1 = V.fromList [-1, 1, -1]
  let v2 = V.fromList [1, -1, -1]
  let v3 = V.fromList [1, 1, -1]
  let ws = evalRand (train [v1, v2, v3] 4) gen
  return $ evalRand (repeatedUpdate1 ws (V.fromList [1, 1, 1])) gen
