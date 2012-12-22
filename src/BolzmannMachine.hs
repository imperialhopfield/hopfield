{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module BolzmannMachine where
-- | Base Restricted Bolzamann machine.

-- http://en.wikipedia.org/wiki/Restricted_Boltzmann_machine

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


--TODO CHECK IF IT IS OK WITH -1 and 1 instead of 0 and 1
-- start with no biases initially, introduce them after (if needed)
-- if biases are used, they should be normally distributed


lr = 0.1 :: Double -- learning rate

data Mode = Hidden | Visible

notMode :: Mode -> Mode
notMode Hidden  = Visible
notMode Visible = Hidden

getDimension :: Mode -> Weights -> Int
getDimension Hidden ws = V.length $ ws ! 0
getDimension Visible ws = V.length $ ws



updateNeuron :: MonadRandom m => Mode -> Weights -> Pattern -> Int -> m Int
updateNeuron mode ws pat index = do
  r <- getRandomR (0.0, 1.0)
  return $ if (r < a) then 1 else -1
    where
      a = activation . sum $ case mode of
            Hidden  -> [ (ws ! index ! i) *. (pat ! i) | i <- [0 .. p-1] ]
            Visible -> [ (ws ! i ! index) *. (pat ! i) | i <- [0 .. p-1] ]
      p = V.length pat

getCounterPattern:: MonadRandom m => Mode -> Weights -> Pattern -> m Pattern
getCounterPattern mode ws pat
  | Just e <- validVisiblePattern ws pat  = error e
  | otherwise = do
      c_pat <- mapM (updateNeuron (notMode mode) ws pat) [0.. getDimension mode - 1]
      return $ V.fromList c_pat


updateWS:: MonadRandom m => Weights -> Pattern -> m Weights
updateWS ws v = do
  h         <- getCounterPattern Hidden ws v
  v'        <- getCounterPattern Visible ws h
  h'        <- getCounterPattern Hidden ws v'
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
  ws_start <- genWs
  foldM updateWS (vector2D ws_start) pats
    where
      genWs      = replicateM nr_visible . replicateM nr_hidden $ normal 0.0 0.01
      nr_visible = V.length $ pats !! 0


activation :: Double -> Double
activation x = 1.0 / (1.0 - exp (-x))




-- | @validPattern weights pattern@
-- Returns an error string in a Just if the @pattern@ is not compatible
-- with @weights@ and Nothing otherwise.
validHiddenPattern :: Mode -> Weights -> Pattern -> Maybe String
validHiddenPattern mode ws pat
  | getDimension mode /= V.length pat = Just "Size of hidden must match network size"
  | otherwise                   = Nothing


 -- | Generates a number sampled from a random distribution.
normal :: forall m . MonadRandom m => Double -> Double -> m Double
normal m std = do
  r <- DR.runRVar (DR.normal m std) (getRandom :: MonadRandom m => m Word32)
  return r

update1 :: MonadRandom m => Weights -> Pattern -> m Pattern
update1 ws pat = do
  h <- getCounterPattern Hidden ws pat
  getCounterPattern Visible ws h

repeatedUpdate1 :: MonadRandom m => Weights -> Pattern -> m Pattern
repeatedUpdate1 ws pat = repeatUntilEqual (update1 ws) pat

main = do
  gen  <- getStdGen
  let v1 = V.fromList [-1, 1, -1]
  let v2 = V.fromList [1, -1, -1]
  let v3 = V.fromList [1, 1, -1]
  let ws = evalRand (train [v1, v2, v3] 4) gen
  return $ evalRand (repeatedUpdate1 ws (V.fromList [1, 1, 1])) gen
