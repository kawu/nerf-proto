{-# LANGUAGE RecordWildCards #-}

-- | A set of potential trees.  Implemented solutions are suboptimal.
-- They are designed for consistency-checking purposes.

module NLP.Nerf2.Tree.Set
( treeSet
, treeSet'
, treeSet''
) where

import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import qualified Data.Vector as V

import NLP.Nerf2.Types
-- import NLP.Nerf2.Active
import NLP.Nerf2.SpanDiv
import NLP.Nerf2.Tree
import qualified NLP.Nerf2.CFG as C

-- data Base = Base
--     { sent      :: Sent
--     , cfg       :: C.CFG
--     , active    :: Active }

-- | A set of potential trees spanned over the given symbol and positions.
treeSet :: Either N T -> Pos -> Pos -> Nerf [Tree]
treeSet x i j = do
    xs <- treeSet' x i j
    ys <- treeSet'' x i j
    return $ xs ++ ys

-- | A set of potential 'Fork' trees spanned over the given symbol
-- and positions.
treeSet' :: Either N T -> Pos -> Pos -> Nerf [Tree]
treeSet' (Left x) i j = do
    [ setOn u
    | u <- C.perTopU cfg x ]
  where
    setOn u = map (Branch x) <$> treeSet'' (C.down u) i j

treeSet' _ (Right _) _ _ = return []

-- | A set of potential 'Branch' and 'Leaf' trees spanned over the
-- given symbol and positions.
treeSet'' :: Base -> Either N T -> Pos -> Pos -> [Tree]
treeSet'' base@Base{..} (Left x) i j
    | i == j    = []
    | otherwise =
        [ Fork x l p
        | r <- C.perTopB cfg x
        , k <- divTop active i j
        , l <- treeSet base (C.left r) i k
        , p <- treeSet base (C.right r) (k+1) j ]
treeSet'' Base{..} (Right x) i j
    | i == j    = maybeToList $ do
        y <- fst <$> sent V.!? i
        if x == y
            then Just (Leaf x i)
            else Nothing
    | otherwise = []
