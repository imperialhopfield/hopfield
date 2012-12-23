{-# LANGUAGE PatternGuards #-}

module Common where


import Data.List
import Data.Vector (Vector)

type Weights = Vector (Vector Double)
type Pattern = Vector Int


getPatternFromList :: [Pattern] -> Pattern -> Either Pattern Int
getPatternFromList pats pat =
  case m_index of
        Nothing    -> Left pat
        Just index -> Right index
  where m_index = pat `elemIndex` pats

