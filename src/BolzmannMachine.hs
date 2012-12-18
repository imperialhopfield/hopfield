module BolzmannMachine where

import           Data.Functor
import           Data.List
--import           Data.Random
import           Control.Monad
import           Control.Monad.Random
import           Data.Vector ((!))
import qualified Data.Vector as V
import qualified Numeric.Container as NC

import Hopfield
import Util

-- start with no biases initially, introduce them after

-- note that v and h do not need to have the same state
-- state h v
-- energy

-- probability
-- sample h given v
-- sample v given h
-- update weigths between hidden and visible layer

lr = 0.1 :: Double -- learning rate

-- todo checks for Weigths. Here there are different
-- todo checks for patterns. Here there are different
-- todo see order of updates


-- TODO check size h and width ws same
updateHidden:: MonadRandom m => Weights -> Pattern -> Int -> m Int
updateHidden ws v index= do
    r <- getRandomR (0.0, 1.0)
    return $ if  (r < a) then 1 else 0
      where
        a = activation (sum [ (ws ! i ! index) *. (v ! i) | i <- [0 .. p-1] ])
        p = V.length v


-- TODO check size h and length ws same
updateVisible:: MonadRandom m => Weights -> Pattern -> Int -> m Int
updateVisible ws h index = do
    r <- getRandomR (0.0, 1.0)
    return $ if  (r < a) then 1 else 0
      where
        a = activation (sum [ (ws ! index ! i) *. (h ! i) | i <- [0 .. p-1] ])
        p = V.length h


getHidden:: MonadRandom m => Weights -> Pattern -> m Pattern
getHidden ws v = do
      h <- mapM (updateHidden ws v) [0.. (V.length $ ws ! 0) - 1]
      return $ V.fromList h


getVisible:: MonadRandom m => Weights -> Pattern -> m Pattern
getVisible ws h = do
      v <- mapM (updateVisible ws h) [0.. (V.length ws) - 1]
      return $ V.fromList v


updateWS:: MonadRandom m => Weights -> Pattern -> m Weights
updateWS ws v = do
    h    <- getHidden ws v
    v'   <- getVisible ws h
    h'   <- getHidden ws v'
    v_d  <- return $ V.fromList $ map fromIntegral (V.toList v)
    v_d' <- return $ V.fromList $ map fromIntegral (V.toList v')
    h_d  <- return $ V.fromList $ map fromIntegral (V.toList h)
    h_d' <- return $ V.fromList $ map fromIntegral (V.toList h')
    pos  <- return $ NC.toLists $ (fromDataVector v_d) `NC.outer` (fromDataVector h_d)
    neg  <- return $ NC.toLists $ (fromDataVector v_d') `NC.outer` (fromDataVector h_d')
    dws  <- return $ combine (-) pos neg
    ws_f <-  return $ combine (+) (list2D ws) dws
    final <- return $ map (map (\x -> x * lr)) ws_f
    return $ vector2D final

train :: MonadRandom m => [Pattern] -> Int -> m Weights
train pats nr_hidden = do
  ws_start <- ws_start''
  foldM updateWS ws_start pats
    where ws_start'  = take p (repeat $ take nr_hidden $ repeat $ normal 0.0 0.01)
          ws_start'' = liftM vector2D (sequence $ map sequence ws_start')
          p = length pats

activation :: Double -> Double
activation x = 1.0 / (1.0 - exp (-x))


-- stack overflow code

-- |Generates uniform random variables.
unif :: (MonadRandom m) => m Double
unif = getRandomR (0,1)

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
    (x,_) <- stdNormals
    return x

-- |Generate a sample from the standard normal distribution with a given
--  mean and variance.
normal :: (MonadRandom m) => Double -> Double -> m Double
normal mu sigma = do
    x <- stdNormal
    return $ mu + sigma * x