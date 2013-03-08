module NLP.Nerf2.Tree.Phi
( phiTree
) where

import Control.Applicative ((<$>))

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
    , phiBinary r
    , phiTreeP l
    , phiTreeP p ]
  where
    r = CFG.Binary x (sym l) (sym p)
    sym = Left . labelP
phiTreeP (BranchP x t i j) = product <$> sequence
    [ phiNode x i j
    , phiUnary u
    , phiTreeP t ]
  where
    u = CFG.Unary x (sym t)
    sym = Left . labelP
phiTreeP (LeafP x i) =
    let fromBool b = if b then 1 else 0
    in  fromBool <$> inputHas i x
