{-# LANGUAGE RecordWildCards #-}

-- | A set of potential trees.  Implemented solutions are suboptimal.
-- They are designed for consistency-checking purposes.

module NLP.Nerf2.Tree.Set
( treeSet
, treeSet'
, treeSet''
) where

import Control.Monad.Trans.Class (lift)

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import NLP.Nerf2.SpanDiv
import NLP.Nerf2.Tree
import qualified NLP.Nerf2.CFG as C
import qualified NLP.Nerf2.ListT as L

-- | A set of potential trees spanned over the given symbol and positions.
treeSet :: Either N T -> Pos -> Pos -> L.ListT Nerf Tree
treeSet x i j = L.append
    (treeSet' x i j)
    (treeSet'' x i j)

-- | A set of potential 'Fork' trees spanned over the given symbol
-- and positions.
treeSet' :: Either N T -> Pos -> Pos -> L.ListT Nerf Tree
treeSet' (Left x) i j = do
    cfg <- lift nerfCFG
    u   <- L.liftList $ C.perTopU cfg x
    t   <- treeSet'' (C.down u) i j
    return $ Branch x t
treeSet' (Right _) _ _ = L.empty

-- | A set of potential 'Branch' and 'Leaf' trees spanned over the
-- given symbol and positions.
treeSet'' :: Either N T -> Pos -> Pos -> L.ListT Nerf Tree
treeSet'' (Left x) i j
    | i == j    = L.empty
    | otherwise = do
        cfg <- lift nerfCFG
        r   <- L.liftList $ C.perTopB cfg x
        k   <- divTop i j
        l   <- treeSet (C.left r) i k
        p   <- treeSet (C.right r) (k+1) j
        return $ Fork x l p
treeSet'' (Right x) i j
    | i == j    = lift (inputHas i x) >>= \b -> if b
        then L.singleton $ Leaf x i
        else L.empty
    | otherwise = L.empty
