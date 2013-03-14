-- | An internal, monadic interface.

module NLP.Nerf2.Monad
(
-- * Types
  Nerf
, NerfD (NerfD)
, runNerf
-- * Basic
, labelNum
, labelVect
, labels
-- * Context free grammar
, nerfCFG
-- * Active set
, activeSet
, isActive
, activeCond
-- * Input
, input
, inputHas
-- * Potential
, phiNode
, phiBinary
, phiUnary

-- * Experimental
, phiNodeMap
, binaryNN
, binaryNT
, binaryTN
, binaryTT
) where

import Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Control.Monad.Reader as R
-- import qualified Control.Monad.Trans.State.Strict as S

import NLP.Nerf2.Types
import qualified NLP.Nerf2.CFG as C 

-- | Nerf data.
data NerfD = NerfD
    { cfg       :: C.CFG
    , sent      :: Sent
    , active    :: Active
    -- TODO: Of cource phi functions have to reimplemented.
    , phiNodeM      :: M.Map (N, Pos, Pos) LogReal
    , phiUnaryM     :: M.Map C.Unary LogReal
    , phiBinaryM    :: M.Map C.Binary LogReal }
    deriving (Show)

-- | A Nerf monad.  Do we really gain anything by using the monadic
-- interface?  I don't know, so lets make it an experiment.
type Nerf = R.Reader NerfD

-- | Run the `Nerf` monad.
runNerf :: NerfD -> Nerf a -> a
runNerf nd nerf = R.runReader nerf nd

labelVect :: Nerf (U.Vector N)
labelVect = undefined

labelNum :: Nerf Int
labelNum = U.length <$> labelVect

labels :: Nerf [N]
labels = U.toList <$> labelVect

-- | A context free grammar.
nerfCFG :: Nerf C.CFG
nerfCFG = R.asks cfg

-- | Set of active spans.
activeSet :: Nerf Active
activeSet = R.asks active

-- | Is a span active?
isActive :: Pos -> Pos -> Nerf Bool
isActive i j = S.member (i, j) <$> activeSet

-- | Conditional execution.
activeCond :: Pos -> Pos -> Nerf a -> Nerf a -> Nerf a
activeCond i j n m = isActive i j >>= \is ->
    if is then m else n

-- | Potential of a tree node within the context.
phiNode :: N -> Pos -> Pos -> Nerf LogReal
phiNode x i j = M.findWithDefault 1 (x, i, j) <$> R.asks phiNodeM

-- | Potential of a binary rule.
phiBinary :: C.Binary -> Nerf LogReal
phiBinary r = M.findWithDefault 1 r <$> R.asks phiBinaryM

-- | Potential of an unary rule.
phiUnary :: C.Unary -> Nerf LogReal
phiUnary u = M.findWithDefault 1 u <$> R.asks phiUnaryM

-- | Does the input sentence have the particular terminal
-- on the particular position?
inputHas :: Pos -> T -> Nerf Bool
inputHas i x = do
    s <- R.asks sent
    return $ case s V.!? i of
        Just (y, _) -> x == y
        Nothing     -> False

-- | Get input.
input :: Nerf Sent
input = R.asks sent

----------------------------------------------------------------------------
-- Experimental section.
--
-- At different stages of alpha, beta and other computations we assume that
-- some special constructions (for example, a list of binary rules together
-- with corresponding potentials for every top, nonterminal symbol) are
-- present.
--
-- These constructions should be updated when some other part of the Nerf
-- data changes.  For example, when parameter values change, the
-- `perTopB'NN` should also yield a different result.  When a context
-- of a sentence is changed, values of node potentials should also change.
--
-- We have a kind of a dependency graph between different parts of the Nerf
-- state.  It would be nice to express somehow these dependencies so that
-- dependent values would be automatically updated when dependencies change. 
--
-- For now, we just assume that appropriate constructions have appropriate
-- values, regardless of how they are computed. 
----------------------------------------------------------------------------

phiNodeMap :: Nerf (M.Map (Pos, Pos) RVect)
phiNodeMap = undefined

-- | A set of (left, top, right, binary rule potential) tuples.
-- TODO: Optimize; we would iteration over this vector as fast
-- as possible.
binaryNN :: Nerf (V.Vector (N, N, N, LogReal))
binaryNN = undefined

-- | A set of (left, top, potential) tuples for a given terminal.
binaryNT :: Nerf (T -> V.Vector (N, N, LogReal))
binaryNT = undefined

-- | A set of (top, right, potential) tuples for a given terminal.
binaryTN :: Nerf (T -> V.Vector (N, N, LogReal))
binaryTN = undefined

-- | A set of (top, potential) tuples for a given left and right terminals.
binaryTT :: Nerf (T -> T -> V.Vector (N, LogReal))
binaryTT = undefined
