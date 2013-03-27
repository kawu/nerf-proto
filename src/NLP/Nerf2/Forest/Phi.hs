{-# LANGUAGE RecordWildCards #-}

-- | A potential of a forest.

module NLP.Nerf2.Forest.Phi
( phiForest
, norm
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Tree
import NLP.Nerf2.Tree.Phi
import NLP.Nerf2.Forest.Set
import qualified NLP.Nerf2.Env as Env

-- | Potential of a forest.
phiForest :: Env.InSent e => e -> Forest -> LogReal
phiForest env ts
    = product [ phiTree env t | t <- ts ]
    * product [ Env.phiEdge env (lb t) (lb t')
              | (t, t') <- pairs ts ]
  where
    lb Leaf{..} = error "phiForest: leaf in a forest"
    lb t        = label t
    pairs xs    = zip xs (tail xs)

-- | Normalization factor.
norm :: Env.InSent e => e -> LogReal
norm e = sum [phiForest e f | f <- forestSet e]
