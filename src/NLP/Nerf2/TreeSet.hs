{-# LANGUAGE RecordWildCards #-}

-- | A set of potential trees.  Implemented solutions are suboptimal.
-- They are designed for consistency-checking purposes.

module NLP.Nerf2.TreeSet
( treeSet
, treeSet'
, treeSet''
) where

import Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Data.Vector as V

import NLP.Nerf2.Types
import NLP.Nerf2.Active
import NLP.Nerf2.Tree
import qualified NLP.Nerf2.CFG as C

data Base = Base
    { sent      :: Sent
    , cfg       :: C.CFG
    , active    :: Active }

-- | A set of potential trees spanned over the given symbol and positions.
treeSet :: Base -> Either N T -> Pos -> Pos -> [Tree]
treeSet base x i j = treeSet' base x i j ++ treeSet'' base x i j

-- | A set of potential 'Fork' trees spanned over the given symbol
-- and positions.
treeSet' :: Base -> Either N T -> Pos -> Pos -> [Tree]
treeSet' base@Base{..} (Left x) i j =
    [ Branch x t
    | u <- C.perTopU cfg x
    , t <- treeSet'' base (C.down u) i j ]
treeSet' _ (Right _) _ _ = []

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
    | i == j    =
        if Just x == word
            then [Leaf x i]
            else []
    | otherwise = []
  where
    word = fst <$> sent V.!? i

-- | A set of possible divisions of the (i, j) span into two
-- neighboring (i, k) and (k+1, j) active spans.
-- The result is returned in the ascending order.
divTop :: Active -> Pos -> Pos -> [Pos]
divTop active i j =
    [ k | k <- [i .. j - 1]
    , S.member (i, k) active
    , S.member (k+1, j) active ]
