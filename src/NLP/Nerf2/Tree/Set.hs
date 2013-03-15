-- | A set of potential trees.  Implemented solutions are suboptimal.
-- They are designed for consistency-checking purposes.

module NLP.Nerf2.Tree.Set
( treeSet
, treeSet'
, treeSet''
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Active
import NLP.Nerf2.Tree
import qualified NLP.Nerf2.CFG as C
import qualified NLP.Nerf2.Env as Env

-- | A set of potential trees spanned over the given symbol and positions.
treeSet :: Env.InSent e => e -> Either N T -> Pos -> Pos -> [Tree]
treeSet env x i j = treeSet' env x i j ++ treeSet'' env x i j

-- | A set of potential 'Branch' trees spanned over the given symbol
-- and positions.
treeSet' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> [Tree]
treeSet' env n i j = if Env.isActive env i j
    then treeSetI' env n i j
    else []

treeSetI' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> [Tree]
treeSetI' env (Left x) i j =
    [ Branch x t
    | u <- Env.perTopU env x
    , t <- treeSet'' env (C.down u) i j ]
treeSetI' _ (Right _) _ _ = []

-- | A set of potential 'Fork' and 'Leaf' trees spanned over the
-- given symbol and positions.
treeSet'' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> [Tree]
treeSet'' env n i j = if Env.isActive env i j
    then treeSetI'' env n i j
    else []

treeSetI'' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> [Tree]
treeSetI'' env (Left x) i j
    | i == j    = []
    | otherwise =
        [ Fork x l p
        | r <- Env.perTopB env x
        , k <- divTopE env i j
        , l <- treeSet env (C.left r) i k
        , p <- treeSet env (C.right r) (k+1) j ]
treeSetI'' env (Right x) i j
    | i == j    = if Env.inputHas env i x
        then [Leaf x i]
        else []
    | otherwise = []
