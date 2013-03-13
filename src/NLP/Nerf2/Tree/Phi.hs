{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf2.Tree.Phi
( phiTree
) where

import Control.Applicative ((<$>))

import NLP.Nerf2.Types
import NLP.Nerf2.Tree
import NLP.Nerf2.Monad
import qualified NLP.Nerf2.CFG as CFG 

-- | A potential of a tree.
phiTree :: Tree -> Nerf LogReal
phiTree t = phiTreeP (posify t)

-- | A potential of a tree with positions.
phiTreeP :: TreeP -> Nerf LogReal
phiTreeP (ForkP x l p i j) = product <$> sequence
    [ phiNode x i j
    , phiBinary (CFG.Binary x (symP l) (symP p))
    , phiTreeP l
    , phiTreeP p ]
phiTreeP (BranchP x t i j) = product <$> sequence
    [ phiNode x i j
    , phiUnary (CFG.Unary x (symP t))
    , phiTreeP t ]
phiTreeP (LeafP x i) =
    let fromBool b = if b then 1 else 0
    in  fromBool <$> inputHas i x

-- | Symbol in a root.
symP :: TreeP -> Either N T
symP LeafP{..}  = Right terminalP
symP t          = Left (labelP t)
