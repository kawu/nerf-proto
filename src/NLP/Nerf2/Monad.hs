-- | An internal, monadic interface.

module NLP.Nerf2.Monad
(
-- * Types
  Nerf
, NerfD (NerfD)
, runNerf
-- * Context free grammar
, nerfCFG
-- * Active set
, activeSet
, isActive
, activeCond
-- * Input
, inputHas
-- * Potential
, phiNode
, phiBinary
, phiUnary
) where

import Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Control.Monad.Reader as R
-- import qualified Control.Monad.Trans.State.Strict as S

import NLP.Nerf2.Types
import qualified NLP.Nerf2.CFG as C 

-- | Nerf data.
data NerfD = NerfD
    { cfg       :: C.CFG
    , sent      :: V.Vector T   -- TODO: Include observations
    , active    :: S.Set (Pos, Pos)
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

-- | A context free grammar.
nerfCFG :: Nerf C.CFG
nerfCFG = R.asks cfg

-- | Set of active spans.
activeSet :: Nerf (S.Set (Pos, Pos))
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
        Just y  -> x == y
        Nothing -> False
