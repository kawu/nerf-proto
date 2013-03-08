-- | Probability sums.

module NLP.Nerf2.Tree.Alpha
(
) where

import NLP.Nerf2.Types

alphaRef :: Either N T -> Pos -> Pos -> Nerf LogReal
alphaRef x i j =
