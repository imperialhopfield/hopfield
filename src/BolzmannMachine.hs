import Hopfield
import Numeric.Container

-- start with no biases initially

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
updateHidden:: MonadRandom m => Weights -> Pattern -> Int -> m Double
updateHidden index ws v = do
    r <- getRandomR (0.0, 1.0)
    in  return $ if  (r < a) then 1 else 0
      where
        a = activation (sum [ (ws ! i ! index) *. (v ! i) | i <- [0 .. p-1] ])
        p = V.length v


-- TODO check size h and length ws same
updateVisible:: MonadRandom m => Weights -> Pattern -> Int -> m Double
updateVisible index ws h = do
    r <- getRandomR (0.0, 1.0)
    in  return $ if  (r < a) then 1 else 0
      where
        a = activation (sum [ (ws ! index ! i) *. (h ! i) | i <- [0 .. p-1] ])
        p = V.length h


getHidden:: MonadRandom m => Weigths -> Pattern -> m Pattern
getHidden ws v = do
      h <- mapM (updateVisible ws v) [0.. (V.length ws ! 0) - 1]
      return $ V.fromList h


getVisible:: MonadRandom m => Weigths -> Pattern -> m Pattern
getHidden ws v = do
      v <- mapM (updateHidden ws h) [0.. (V.length ws) - 1]
      return $ V.fromList v


updateWS:: MonadRandom m => Weights -> Pattern -> m Weights
updateWS ws v = lr * (ws + dws)
    where dws = pos - neg
          pos = V.fromList $ fromRows (v outer h)
          neg = V.fromList $ fromRows (v' outer h')
          h  = getHidden ws v
          v' = getVisible ws h
          h' = getHidden ws v'


-- train, update the ws for all patterns

activation :: Double -> Double
activation x = 1 ./. (1 - exp (-x))

