-- | An internal, monadic interface.

module NLP.Nerf2.Monad
(
-- * Types
  Nerf
-- * Context free grammar
, nerfCFG
-- * Active set
, activeSet
, isActive
-- * Input
, inputHas
-- * Potential
, phiNode
, phiBinary
, phiUnary
) where

import Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Control.Monad.State.Strict as ST

import NLP.Nerf2.Types
import qualified NLP.Nerf2.CFG as C 

-- | A Nerf monad.  Do we really gain anything by using the monadic
-- interface?  I don't know, but lets make an experiment.
type Nerf = ST.State ()

-- | A context free grammar.
nerfCFG :: Nerf C.CFG
nerfCFG = undefined

-- | Set of active spans.
activeSet :: Nerf (S.Set (Pos, Pos))
activeSet = undefined

-- | Is a span active?
isActive :: Pos -> Pos -> Nerf Bool
isActive i j = S.member (i, j) <$> activeSet

-- | Potential of a tree node within the context.
phiNode :: N -> Pos -> Pos -> Nerf LogReal
phiNode = undefined

-- | Potential of a binary rule.
phiBinary :: C.Binary -> Nerf LogReal
phiBinary = undefined

-- | Potential of an unary rule.
phiUnary :: C.Unary -> Nerf LogReal
phiUnary = undefined

-- | Does the input sentence have the particular terminal
-- on the particular position?
inputHas :: Pos -> T -> Nerf Bool
inputHas = undefined
