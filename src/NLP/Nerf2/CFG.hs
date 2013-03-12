{-# LANGUAGE RecordWildCards #-}

-- | A context free grammar.

module NLP.Nerf2.CFG
( CFG (..)
, Unary (..)
, Binary (..)
, perTopU
, perDown
, perTopB
, perLeft
, perRight
) where

import NLP.Nerf2.Types

-- TODO: implement a vector representation of a set.
-- We want to specify that some structures are (ordered)
-- sets, but we also want, e.g., to be able to quickly
-- transform the set into a list.
-- You should also make some tests which prove that
-- the vector implementation is actually faster.
import qualified Data.Set as S

-- | A context free grammar.
data CFG = CFG
    { unary     :: S.Set Unary
    , binary    :: S.Set Binary
    , nsyms     :: S.Set N
    , tsyms     :: S.Set T
    , start     :: S.Set N }

-- | An unary CFG rule.
data Unary = Unary
    { topU  :: N
    , down  :: Either N T }
    deriving (Show, Eq, Ord)

-- | A binary CFG rule.
data Binary = Binary
    { topB  :: N
    , left  :: Either N T
    , right :: Either N T }
    deriving (Show, Eq, Ord)

-- | A set of unary rules with the given top symbol.
perTopU :: CFG -> N -> [Unary]
perTopU CFG{..} x = filter ((==x).topU) (S.toList unary)

-- | A set of unary rules with the given down symbol.
perDown :: CFG -> Either N T -> [Unary]
perDown CFG{..} x = filter ((==x).down) (S.toList unary)

-- | A set of binary rules with the given top symbol.
perTopB :: CFG -> N -> [Binary]
perTopB CFG{..} x = filter ((==x).topB) (S.toList binary)

-- | A set of binary rules with the given left symbol.
perLeft :: CFG -> Either N T -> [Binary]
perLeft CFG{..} x = filter ((==x).left) (S.toList binary)

-- | A set of binary rules with the given right symbol.
perRight :: CFG -> Either N T -> [Binary]
perRight CFG{..} x = filter ((==x).right) (S.toList binary)
