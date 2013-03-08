-- | An internal, monadic interface.

module NLP.Nerf2.Monad
(
-- * Types
  Nerf
, LogReal
-- * Input
, inputHas
-- * Potential
, phiNode
, phiBinary
, phiUnary
) where

import qualified Data.Number.LogFloat as L
import qualified Control.Monad.State.Strict as ST

import NLP.Nerf2.Types
import qualified NLP.Nerf2.CFG as CFG 

-- | A Nerf monad.  Do we really gain anything by using the monadic
-- interface?  I don't know, but lets make an experiment.
type Nerf = ST.State ()

-- | A real value.
type LogReal = L.LogFloat

-- | Potential of a tree node within the context.
phiNode :: N -> Pos -> Pos -> Nerf LogReal
phiNode = undefined

-- | Potential of a binary rule.
phiBinary :: CFG.Binary -> Nerf LogReal
phiBinary = undefined

-- | Potential of an unary rule.
phiUnary :: CFG.Unary -> Nerf LogReal
phiUnary = undefined

-- | Does the input sentence have the particular terminal
-- on the particular position?
inputHas :: Pos -> T -> Nerf Bool
inputHas = undefined
