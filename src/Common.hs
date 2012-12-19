{-# LANGUAGE PatternGuards #-}

module Common where


import           Data.List
import           Data.Maybe
import           Data.Number.Erf
import           Data.Vector (Vector, (!))
import           Data.Vector.Generic.Mutable (write)
import qualified Data.Vector as V
import           Control.Monad.Random (MonadRandom)
import           Util

type Weights = Vector (Vector Double)
type Pattern = Vector Int


-- | Encapsulates the network weights together with the patterns that generate
-- it with the patterns which generate it
data HopfieldData = HopfieldData {
    weights :: Weights    -- ^ the weights of the network
  , patterns :: [Pattern] -- ^ the patterns which were used to train it
}
  deriving(Show)

-- General update and repeated update to be used by both Hopfield and

update :: MonadRandom m => (Weights -> Pattern-> Maybe String)
                            -> Weights -> Pattern -> m Pattern
update f ws pat
  | Just e <- f ws pat        = error e
  -- | Just e <- validWeights ws = error e
  | otherwise                 = update' ws pat


-- | @repeatedUpdate weights pattern@: Performs repeated updates on the given
-- pattern until it reaches a stable state with respect to the Hopfield network
-- (represented by @weights@).
-- Pre: @length weights == length pattern@
repeatedUpdate :: (MonadRandom m) => (Weights -> Pattern-> Maybe String)
                                      -> Weights -> Pattern -> m Pattern
repeatedUpdate f ws pat
  | Just e <- f ws pat        = error e
  -- | Just e <- validWeights ws = error e
  | otherwise                 = repeatUntilEqual (update' ws) pat

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
matchPattern :: MonadRandom m => (Weights -> Pattern-> Maybe String)
                                 -> HopfieldData -> Pattern -> m (Either Pattern Int)
matchPattern f (HopfieldData ws pats) pat
  -- | Just e <- validWeights ws = error e
  | Just e <- f ws pat        = error e
  | otherwise
    = do
      converged_pattern <- repeatedUpdate f ws pat
      let m_index = converged_pattern `elemIndex` pats
      case m_index of
        Nothing    -> return $ Left converged_pattern
        Just index -> return $ Right index


-- | @getUpdatables ws pat@. Given a Hopfield network represented by ws, returns
-- a list of paris comprising of the updatable neurons (represented by the index
-- in the pattern) and their new, changed value.
-- No check is performed in this function for efficiency reasons: the checks
-- are expensive and are done in update, before update'.
-- Any other caller should ensure that ws and pat are compatible and valid
-- (by calling @valid)
getUpdatables:: Weights -> Pattern -> [(Int, Int)]
getUpdatables ws pat = updatables
  where
    updatables = [ (i, computeH ws pat i) | (i, x_i) <- zip [0..] (V.toList pat), computeH ws pat i /= x_i ]


computeH :: Weights -> Pattern -> Int -> Int
computeH ws pat i = if sum [ (ws ! i ! j) *. (pat ! j)
                     | j <- [0 .. p-1] ] >= 0 then 1 else -1
              where   p = V.length pat


-- | @updateViaIndex updatables index pat@ Takes the new value of the neuron
-- represented by @index@ and changes its value in pat, returning the
-- changed, updated pattern.
-- The caller must ensure that index is smalupdateViaIndex updatables index patler than the length of updatables
updateViaIndex :: [(Int, Int)] -> Int -> Pattern -> Pattern
updateViaIndex updatables index pat =
  case updatables of
    [] -> pat
    _  -> V.modify (\v -> write v index (fromJust $ lookup index updatables)) pat


-- | Same as 'update', without size/dimension check, for performance.
update' :: MonadRandom m => Weights -> Pattern -> m Pattern
update' ws pat = do
      index <- randomElem $ map fst updatables
      return $ updateViaIndex updatables index pat
  where
    updatables = getUpdatables ws pat