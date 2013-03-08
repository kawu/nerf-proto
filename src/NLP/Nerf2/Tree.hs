-- | A binary tree data type.

module NLP.Nerf2.Tree
( Tree (..)
) where

import NLP.Nerf2.Types

-- | A binary tree.
data Tree
    = Fork
        { label :: N
        , left  :: Tree
        , right :: Tree }
    | Branch
        { label :: N
        , down  :: Tree }
    | Leaf
        { terminal  :: T
        , pos       :: Pos }
    deriving (Show, Eq, Ord)
