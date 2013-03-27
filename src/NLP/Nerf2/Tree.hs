-- | A binary tree data type.

module NLP.Nerf2.Tree
( -- * Plain tree
  Tree (..)
, beg
, end
, span
, isFork
, isBranch
, isLeaf
-- * Tree with positions
, TreeP (..)
, begP
, endP
, posify
, unPosify
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

isFork :: Tree -> Bool
isFork Fork{} = True
isFork _        = False

isBranch :: Tree -> Bool
isBranch Branch{} = True
isBranch _          = False

isLeaf :: Tree -> Bool
isLeaf Leaf{} = True
isLeaf _        = False

-- | First position covered by a tree.
beg :: Tree -> Pos
beg (Fork _ t _) = beg t
beg (Branch _ t) = beg t
beg (Leaf _ i)   = i

-- | Last position covered by a tree.
end :: Tree -> Pos
end (Fork _ _ t) = end t
end (Branch _ t) = end t
end (Leaf _ i)   = i

-- | A binary tree with additional information about
-- positions in internal nodes.
data TreeP
    = ForkP
        { labelP    :: N
        , leftP     :: TreeP
        , rightP    :: TreeP
        , _begP     :: Pos
        , _endP     :: Pos }
    | BranchP
        { labelP    :: N
        , downP     :: TreeP
        , _begP     :: Pos
        , _endP     :: Pos }
    | LeafP
        { terminalP :: T
        , posP      :: Pos }
    deriving (Show, Eq, Ord)

-- | First position covered by a position tree.
begP :: TreeP -> Pos
begP (LeafP _ i) = i
begP t           = _begP t

-- | Last position covered by a position tree.
endP :: TreeP -> Pos
endP (LeafP _ i) = i
endP t           = _endP t

-- | Make a TreeP from a Tree.
posify :: Tree -> TreeP
posify (Fork x l p) =
    let l' = posify l
        p' = posify p
    in  ForkP x l' p' (begP l') (endP p')
posify (Branch x t) =
    let t' = posify t
    in  BranchP x t' (begP t') (endP t')
posify (Leaf x i) = LeafP x i

-- | Make a Tree from a TreeP.
unPosify :: TreeP -> Tree
unPosify (ForkP x l p _ _) = Fork x (unPosify l) (unPosify p)
unPosify (BranchP x t _ _) = Branch x (unPosify t)
unPosify (LeafP x i)       = Leaf x i
