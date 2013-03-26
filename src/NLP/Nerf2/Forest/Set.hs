-- | A set of forests.

module NLP.Nerf2.Forest.Set
( Forest
, forestSetF
, forestSetF'
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Tree
import NLP.Nerf2.Tree.Set
import qualified NLP.Nerf2.Env as Env

-- | A forest is a list of trees.  Each tree in the forest has
-- a branch or a fork outgoing from the tree root.
-- TODO: It would be nice to encode this property on the type-level.
-- Would it be possible/easy?
type Forest = [Tree]

-- -- | `forestSetF env x i` is a set of forests, where:
-- -- * `x` is a label in a root of the rightmost tree,
-- -- * A forest is spanned over the (0, i) range.
-- forestSetF :: Env.InSent e => e -> N -> Pos -> [Forest]
-- forestSetF e x i
--     | i < 0                 = [[]]
--     | not (Env.isStart e x) = []
--     | otherwise             =  forestSetF e x (i-1)
--                             ++ forestSetF' e x i
-- 
-- -- | `forestSetF' env x i` is a set of forests, where:
-- -- * `x` is a label in a root of the rightmost tree,
-- -- * A forest is spanned over the (0, i) range and it contains
-- --   a tree spanned over the (k, i) range for some k <= i.
-- forestSetF' :: Env.InSent e => e -> N -> Pos -> [Forest]
-- forestSetF' e x i
--     | not (Env.isStart e x) = []
--     | otherwise             =
--         [ f ++ [t]
--         | k <- [0 .. i]
--         , t <- treeSet e (Left x) k i
--         , y <- Env.begLabels e
--         , f <- forestSetF e y (k - 1) ]

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
forestSetFA e i
    | i < 0     = [[]]
    | otherwise = [f | x <- Env.begLabels e, f <- forestSetF e x i]
