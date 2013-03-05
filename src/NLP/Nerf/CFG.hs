-- | A context free grammar.

module NLP.Nerf.CFG
(
) where

-- TODO: implement a vector representation of a set.
-- We want to specify that some structures are (ordered)
-- sets, but we also want, e.g., to be able to quickly
-- transform the set into a list.
-- You should also make some tests which prove that
-- the vector implementation is actually faster.
import Data.Set as S

-- | A context free grammar.
data CFG n t = CFG
    { unary     :: S.Set (Unary n t)
    , binary    :: S.Set (Binary n t)
    , nsyms     :: S.Set n
    , tsyms     :: S.Set t
    , start     :: S.Set n }

-- | An unary CFG rule.
data Unary n t = Unary
    { topU  :: n
    , down  :: Either n t }
    deriving (Show, Eq, Ord)

-- | A binary CFG rule.
data Binary n t = Binary
    { topB  :: n
    , left  :: Either n t
    , right :: Either n t }
    deriving (Show, Eq, Ord)
