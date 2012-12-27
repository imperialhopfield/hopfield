{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Base Hopfield model, providing training and running.
module Hopfield (
    Weights
  , Pattern
  -- * Hopfield data structure
  , HopfieldData ()
  , weights
  , patterns
  , buildHopfieldData
  -- * Running
  , update
  , computeH
  , getUpdatables
  , updateViaIndex
  , repeatedUpdate
  , matchPattern
  -- * Energy
  , energy
  -- * Basin of attraction
  , measurePatternBasin
) where


import           Data.List
import           Data.Maybe
import           Data.Number.Erf
import           Data.Random.Distribution.Uniform (stdUniform)
import           Data.Random.Extras (sample)
import           Data.RVar (runRVar)
import           Data.Vector (Vector, (!))
import           Data.Vector.Generic.Mutable (write)
import qualified Data.Vector as V
import           Data.Word (Word32)
import           Common
import           Control.Monad.Random (MonadRandom)
import           Control.Monad.Random.Class (getRandom)
import           Util


--make Hopefield data implement show
-- | Encapsulates the network weights together with the patterns that generate
-- it with the patterns which generate it
data HopfieldData = HopfieldData {
    weights :: Weights    -- ^ the weights of the network
  , patterns :: [Pattern] -- ^ the patterns which were used to train it
}
  deriving(Show)


-- | @buildHopfieldData patterns@: Takes a list of patterns and
-- builds a Hopfield network (by training) in which these patterns are
-- stable states. The result of this function can be used to run a pattern
-- against the network, by using 'matchPattern'.
buildHopfieldData :: [Pattern] -> HopfieldData
buildHopfieldData []   = error "Train patterns are empty"
buildHopfieldData pats
  | first_len == 0
      = error "Cannot have empty patterns"
  | any (\x -> V.length x /= first_len) pats
      = error "All training patterns must have the same length"
  | otherwise
      = HopfieldData (train pats) pats
  where
    first_len = V.length (head pats)


-- | @train patterns@: Trains and constructs network given a list of patterns
-- which are used to build the weight matrix. As a consequence, they will be
-- stable points in the network (by construction).
train :: [Pattern] -> Weights
train pats = vector2D ws
  -- No need to check pats ws size, buildHopfieldData does it
  where
    ws = [ [ w i j ./. n | j <- [0 .. n-1] ] | i <- [0 .. n-1] ]
    w i j
      | i == j    = 0
      | otherwise = sum [ (pat ! i) * (pat ! j) | pat <- pats ]
    n = V.length (head pats)


-- | @getUpdatables ws pat@. Given a Hopfield network represented by ws, returns
-- a list of pairs comprising of the updatable neurons (represented by the index
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


-- | @update weights pattern@: Applies the update rule on @pattern@ for the
-- first updatable neuron given the Hopfield network (represented by @weights@).
--
-- Pre: @length weights == length pattern@
update :: MonadRandom m => Weights -> Pattern -> m Pattern
update ws pat
  | Just e <- validPattern ws pat = error e
  | Just e <- validWeights ws     = error e
  | otherwise                     = update' ws pat


-- | @repeatedUpdate weights pattern@: Performs repeated updates on the given
-- pattern until it reaches a stable state with respect to the Hopfield network
-- (represented by @weights@).
-- Pre: @length weights == length pattern@
repeatedUpdate :: (MonadRandom m) => Weights -> Pattern -> m Pattern
repeatedUpdate ws pat
  | Just e <- validPattern ws pat = error e
  | Just e <- validWeights ws     = error e
  | otherwise                     = repeatUntilEqual (update' ws) pat


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
matchPattern :: MonadRandom m => HopfieldData -> Pattern
                                 -> m (Either Pattern Int)
matchPattern (HopfieldData ws pats) pat
  | Just e <- validWeights ws     = error e
  | Just e <- validPattern ws pat = error e
  | otherwise = do
      converged_pattern <- repeatedUpdate ws pat
      return $ findInList pats converged_pattern

-- | Computes the probability of error for one element given a hopfield data
-- structure. Note that I claim that the actuall error of probability depends
-- on this, but is not the whole term
computeError :: HopfieldData -> Double
computeError (HopfieldData _ pats) = 1 ./. 2 * (1 - (erf $ sqrt $ n ./. p))
  where n = V.length $ pats !! 0
        p = length pats


-- | @energy weights pattern@: Computes the energy of a pattern given a Hopfield
-- network (represented by @weights@).
-- Pre: @length weights == length pattern@
energy :: Weights -> Pattern -> Double
energy ws pat
  | Just e <- validWeights ws     = error e
  | Just e <- validPattern ws pat = error e
  | otherwise                     = s / (-2.0)
    where
      p     = V.length pat
      w i j = ws ! i ! j
      x i   = pat ! i
      s = sum [ w i j *. (x i * x j) | i <- [0 .. p-1], j <- [0 .. p-1] ]


-- | @validPattern weights pattern@
-- Returns an error string in a Just if the @pattern@ is not compatible
-- with @weights@ and Nothing otherwise.
validPattern :: Weights -> Pattern -> Maybe String
validPattern ws pat
  | V.length ws /= V.length pat = Just "Pattern size must match network size"
  | otherwise                   = Nothing

-- Checks the validity of a weight matrix by ensuring:
-- * It is non-empty
--
-- * It is square
--
-- * It is symmetric
--
-- * All diagonal elements must be zero
validWeights :: Weights -> Maybe String
validWeights ws
  | n == 0
    = Just "Weight matrix must be non-empty"
  | any (\x -> V.length x /= n) $ V.toList ws
    = Just "Weight matrix has to be a square matrix"
  | any (/= 0) [ ws ! i ! i | i <- [0..n-1] ]
    = Just "Weight matrix first diagonal must be zero"
  | not $ and [ (ws ! i ! j) == (ws ! j ! i) | i <- [0..n-1], j <- [0..n-1] ]
    = Just "Weight matrix must be symmetric"
  | null ([abs (ws ! i ! j) > 1 | i <- [0..n-1], j <- [0..n-1] ])
      = Just "Weights should be between (-1, 1)"
  | otherwise = Nothing
  where
    n = V.length ws


-- Generate list of states within hamming distance r of the given pattern
withinHammingRadius :: Pattern -> Int -> [Pattern]
withinHammingRadius pat r = map (V.fromList . multByPat) coeffsList
  where
    n                = V.length pat
    perms            = sequence $ replicate n [1, -1]
    withinDist       = null . (drop r) . (filter (== (-1)))
    coeffsList       = filter withinDist perms
    multByPat coeffs = zipWith (*) coeffs (V.toList pat)


-- Percentage of sampled patterns within hamming distance 'r' from 'pat' which
-- converge to 'pat'
-- pre: pattern of same size as network
samplePatternBasin :: forall m . MonadRandom m => HopfieldData -> Pattern -> Int -> m Double
samplePatternBasin hs pat r = do
  let rSamples      =  sample 100 $ withinHammingRadius pat r
  samples           <- runRVar rSamples (getRandom :: m Word32)
  convergedPatterns <- mapM (repeatedUpdate $ weights hs) samples
  let numConverging =  length $ filter (==pat) convergedPatterns

  return $ numConverging ./. (length samples)


-- Measures pattern's basin of attraction using the Storkey-Valabregue method
-- pre: pattern of same size as network
measurePatternBasin :: (MonadRandom m) => HopfieldData -> Pattern -> m Int
measurePatternBasin hs pat = do
  t_mus <- mapM (samplePatternBasin hs pat)  [1..n]
  return $ fromMaybe n $ findIndex (<0.9) t_mus
    where
      n   = V.length pat
