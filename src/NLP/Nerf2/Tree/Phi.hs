{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf2.Tree.Phi
( phiTree
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Tree
import qualified NLP.Nerf2.CFG as CFG 
import qualified NLP.Nerf2.Env as Env

-- | A potential of a tree.
phiTree :: Env.InSent e => e -> Tree -> LogReal
phiTree env t = phiTreeP env (posify t)

-- | A potential of a tree with positions.
phiTreeP :: Env.InSent e => e -> TreeP -> LogReal
phiTreeP env (ForkP x l p i j) = product
    [ Env.phiNode env x i j
    , Env.phiBinary env (CFG.Binary x (symP l) (symP p))
    , phiTreeP env l
    , phiTreeP env p ]
phiTreeP env (BranchP x t i j) = product
    [ Env.phiNode env x i j
    , Env.phiUnary env (CFG.Unary x (symP t))
    , phiTreeP env t ]
phiTreeP env (LeafP x i) =
    let fromBool b = if b then 1 else 0
    in  fromBool $ Env.inputHas env i x

-- | Symbol in a root.
symP :: TreeP -> Either N T
symP LeafP{..}  = Right terminalP
symP t          = Left (labelP t)
