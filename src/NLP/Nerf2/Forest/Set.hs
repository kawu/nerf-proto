-- | A set of forests.

module NLP.Nerf2.Forest.Set
( Forest
-- * Set of forests
, forestSet
, forestSet'
-- * Forward
, forestSetF
, forestSetF'
-- * Backward
, forestSetB
, forestSetB'
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Tree
import NLP.Nerf2.Tree.Set
import qualified NLP.Nerf2.Env as Env

-- | A forest is a list of trees.  Each tree in the forest has
-- a branch or a fork outgoing from the tree root.
-- TODO: It would be nice to encode this property on the type-level.
-- Is it even possible?
type Forest = [Tree]

------------------
-- FORWARD
------------------

-- | `forestSetF env x i` is a set of forests, where:
-- * `x` is a label in a root of the rightmost tree,
-- * A forest is spanned over the (0, i) range.
forestSetF :: Env.InSent e => e -> N -> Pos -> [Forest]
forestSetF e x i
    | i < 0 || not (Env.isStart e x) = []
    | otherwise = forestSetF e x (i-1) ++ forestSetF' e x i

-- | `forestSetF' env x i` is a set of forests, where:
-- * `x` is a label in a root of the rightmost tree,
-- * A forest is spanned over the (0, i) range and it contains
--   a tree spanned over the (k, i) range for some k <= i.
forestSetF' :: Env.InSent e => e -> N -> Pos -> [Forest]
forestSetF' e x i
    | not (Env.isStart e x) = []
    | otherwise             =
        [ f ++ [t]
        | k <- [0 .. i]
        , t <- treeSet e (Left x) k i
        , f <- forestSetFA e (k - 1) ]

-- | `forestSetFA env i` is a set of forests spanned over the (0, i) range.
forestSetFA :: Env.InSent e => e -> Pos -> [Forest]
forestSetFA e i = [] : if i < 0 then [] else
    [f | x <- Env.begLabels e, f <- forestSetF e x i]

-- | A set of forests spanned over the entire sentence.
forestSet :: Env.InSent e => e -> [Forest]
forestSet e = forestSetFA e (Env.inputLength e - 1)

------------------
-- BACKWARD
------------------

-- | `forestSetB env x i` is a set of forests, where:
-- * `x` is a label in a root of the leftmost tree,
-- * A forest is spanned over the (i, n-1) range.
forestSetB :: Env.InSent e => e -> N -> Pos -> [Forest]
forestSetB e x i
    | i >= Env.inputLength e || not (Env.isStart e x) = []
    | otherwise = forestSetB e x (i+1) ++ forestSetB' e x i

-- | `forestSetB' env x i` is a set of forests, where:
-- * `x` is a label in a root of the leftmost tree,
-- * A forest is spanned over the (i, n-1) range and it contains
--   a tree spanned over the (i, k) range for some k >= i.
forestSetB' :: Env.InSent e => e -> N -> Pos -> [Forest]
forestSetB' e x i
    | not (Env.isStart e x) = []
    | otherwise             =
        [ t : f
        | k <- [i .. Env.inputLength e - 1]
        , t <- treeSet e (Left x) i k
        , f <- forestSetBA e (k + 1) ]

-- | `forestSetBA env i` is a set of forests spanned over the (i, n-1) range.
forestSetBA :: Env.InSent e => e -> Pos -> [Forest]
forestSetBA e i = [] : if i >= Env.inputLength e then [] else
    [f | x <- Env.begLabels e, f <- forestSetB e x i]
     
-- | A set of forests spanned over the entire sentence calculated
-- using the backward computations.
forestSet' :: Env.InSent e => e -> [Forest]
forestSet' e = forestSetBA e 0
