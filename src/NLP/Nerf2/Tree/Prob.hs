-- | Computing probability of a tree.

module NLP.Nerf2.Tree.Prob
( probRef
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import NLP.Nerf2.Tree
import NLP.Nerf2.Tree.Phi

-- | Probability of a tree among other trees spanning the same
-- input range.
probRef :: Tree -> Nerf LogReal
probRef t = do
    x <- phiTree t
    z <- normRef (beg t) (end t)    -- normalization factor
    return (x / z)

-- | Normalization factor.  This function is provided only for reference
-- and consistency checking.  Use the 'norm' function in practice.
normRef :: Pos -> Pos -> Nerf LogReal
normRef _ _ = undefined


