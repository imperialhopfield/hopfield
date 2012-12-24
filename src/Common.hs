{-# LANGUAGE PatternGuards #-}

module Common where


import Data.List
import Data.Vector (Vector)

type Weights = Vector (Vector Double)
type Pattern = Vector Int


-- TODO move to Util
-- make it more general

getPatternFromList :: [Pattern] -> Pattern -> Either Pattern Int
getPatternFromList pats pat =
  case m_index of
        Nothing    -> Left pat
        Just index -> Right index
  where m_index = pat `elemIndex` pats

