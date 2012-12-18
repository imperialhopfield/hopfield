module BolzmannMachine where

import           Data.List
import           Data.Random.Distribution.Normal
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

lr = 0.1 -- learning rate

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
    pos  <- return $ (fromDataVector v_d) `NC.outer` (fromDataVector h_d)
    neg  <- return $ (fromDataVector v_d') `NC.outer` (fromDataVector h_d')
    dws  <- return $ (pos::NC.Matrix Double) - (neg:: NC.Matrix Double)
    ws_m <- return $ toMatrix ws
    return $ vector2D $ NC.toLists $ lr * (ws_m + dws)

 --train, update the ws for all patterns
 --
train :: MonadRandom m => [Pattern] -> Int -> m Weights
train pats nr_hidden = foldM updateWS ws_start pats
-- todo remove the Rvar from start matrices
  where ws_start = take p (repeat $ take nr_hidden $ repeat $ normal 0.0 0.01)
        p = length pats

activation :: Double -> Double
activation x = 1.0 / (1.0 - exp (-x))

