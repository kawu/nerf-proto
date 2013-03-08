-- | A binary tree data type.

module NLP.Nerf2.Tree
( Tree (..)
, TreeP (..)
, posify
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

-- | A binary tree with additional information about
-- positions in internal nodes.
data TreeP
    = ForkP
        { labelP    :: N
        , leftP     :: TreeP
        , rightP    :: TreeP
        , begP      :: Pos
        , endP      :: Pos }
    | BranchP
        { labelP    :: N
        , downP     :: TreeP
        , begP      :: Pos
        , endP      :: Pos }
    | LeafP
        { terminalP :: T
        , posP      :: Pos }
    deriving (Show, Eq, Ord)

beg :: TreeP -> Pos
beg (LeafP _ i) = i
beg t           = begP t

end :: TreeP -> Pos
end (LeafP _ i) = i
end t           = endP t

-- | Make a TreeP from a Tree.
posify :: Tree -> TreeP
posify (Fork x l p) =
    let l' = posify l
        p' = posify p
    in  ForkP x l' p' (beg l') (end p')
posify (Branch x t) =
    let t' = posify t
    in  BranchP x t' (beg t') (end t')
posify (Leaf x i) = LeafP x i
