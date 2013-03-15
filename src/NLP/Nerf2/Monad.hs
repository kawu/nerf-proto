{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

-- | An internal, monadic interface.

module NLP.Nerf2.Monad
( Nerf
, runNerf
, activeCond
-- -- * Types
--   Nerf
-- , NerfD (NerfD)
-- , runNerf
-- -- * Basic
-- , labelNum
-- , labelVect
-- , labels
-- -- * Context free grammar
-- , nerfCFG
-- -- * Active set
-- , activeSet
-- , isActive
-- -- * Input
-- , input
-- , inputHas
-- -- * Potential
-- , phiNode
-- , phiBinary
-- , phiUnary
-- 
-- -- * Experimental
-- , phiNodeMap
-- -- ** Unary rules
-- , unaryN
-- , unaryT
-- -- ** Binary rules
-- , binaryNN
-- , binaryNT
-- , binaryTN
-- , binaryTT
) where

import Control.Applicative ((<$>))
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Control.Monad.Reader as R

import NLP.Nerf2.Types
import qualified NLP.Nerf2.Env as Env
import qualified NLP.Nerf2.CFG as C 

-- | A Nerf monad.
type Nerf = R.Reader Env.Layer2

-- | Run the `Nerf` monad.
runNerf :: Env.Layer2 -> Nerf a -> a
runNerf = flip R.runReader

-- | Conditional execution.
activeCond :: Pos -> Pos -> Nerf a -> Nerf a -> Nerf a
activeCond i j n m = do
    isActive <- R.asks (Env.isActive . Env.sentEnv)
    if isActive i j then m else n

-- labelVect :: Nerf (U.Vector N)
-- labelVect = U.fromList . S.toList . C.nsyms <$> R.asks cfg
-- 
-- labelNum :: Nerf Int
-- labelNum = U.length <$> labelVect
-- 
-- labels :: Nerf [N]
-- labels = U.toList <$> labelVect
-- 
-- -- | A context free grammar.
-- nerfCFG :: Nerf C.CFG
-- nerfCFG = R.asks cfg
-- 
-- -- | Set of active spans.
-- activeSet :: Nerf Active
-- activeSet = R.asks active
-- 
-- -- | Is a span active?
-- isActive :: Pos -> Pos -> Nerf Bool
-- isActive i j = S.member (i, j) <$> activeSet
-- 
-- -- | Conditional execution.
-- activeCond :: Pos -> Pos -> Nerf a -> Nerf a -> Nerf a
-- activeCond i j n m = isActive i j >>= \is ->
--     if is then m else n
-- 
-- -- | Potential of a tree node within the context.
-- phiNode :: N -> Pos -> Pos -> Nerf LogReal
-- phiNode x i j = M.findWithDefault 1 (x, i, j) <$> R.asks phiNodeM
-- 
-- -- | Potential of a binary rule.
-- phiBinary :: C.Binary -> Nerf LogReal
-- phiBinary r = M.findWithDefault 1 r <$> R.asks phiBinaryM
-- 
-- -- | Potential of an unary rule.
-- phiUnary :: C.Unary -> Nerf LogReal
-- phiUnary u = M.findWithDefault 1 u <$> R.asks phiUnaryM
-- 
-- -- | Does the input sentence have the particular terminal
-- -- on the particular position?
-- inputHas :: Pos -> T -> Nerf Bool
-- inputHas i x = do
--     s <- R.asks sent
--     return $ case s V.!? i of
--         Just (y, _) -> x == y
--         Nothing     -> False
-- 
-- -- | Get input.
-- input :: Nerf Sent
-- input = R.asks sent
-- 
-- ----------------------------------------------------------------------------
-- -- Experimental section.
-- --
-- -- At different stages of alpha, beta and other computations we assume that
-- -- some special constructions (for example, a list of binary rules together
-- -- with corresponding potentials for every top, nonterminal symbol) are
-- -- present.
-- --
-- -- These constructions should be updated when some other part of the Nerf
-- -- data changes.  For example, when parameter values change, the
-- -- `perTopB'NN` should also yield a different result.  When a context
-- -- of a sentence is changed, values of node potentials should also change.
-- --
-- -- We have a kind of a dependency graph between different parts of the Nerf
-- -- state.  It would be nice to express somehow these dependencies so that
-- -- dependent values would be automatically updated when dependencies change. 
-- --
-- -- For now, we just assume that appropriate constructions have appropriate
-- -- values, regardless of how they are computed. 
-- --
-- -- TODO: Optimize experimental function.  We would like an iteration over
-- -- vectors to be as fast as possible.
-- ----------------------------------------------------------------------------
-- 
-- phiNodeMap :: Nerf (M.Map (Pos, Pos) RVect)
-- phiNodeMap = do
--     act <- S.toList <$> activeSet
--     phi <- mapM phiSpan act
--     return . M.fromList $ zip act phi
--   where
--     phiSpan (i, j) = do
--         xs <- labels
--         U.fromList <$> sequence [phiNode x i j | x <- xs]
-- 
-- -- | A set of (left, top, right, binary rule potential) tuples.
-- binaryNN :: Nerf (V.Vector (N, N, N, LogReal))
-- binaryNN = do
--     rs  <- S.toList . C.binary <$> R.asks cfg
--     phi <- mapM phiBinary rs
--     return $ V.fromList
--         [ (left, top, right, p)
--         | (C.Binary top (Left left) (Left right), p) <- zip rs phi ]
-- 
-- -- | A set of (left, top, potential) tuples for a given terminal.
-- binaryNT :: Nerf (T -> V.Vector (N, N, LogReal))
-- binaryNT = do
--     rs  <- S.toList . C.binary <$> R.asks cfg
--     phi <- mapM phiBinary rs
--     return . onKey . fmap V.fromList $ M.fromListWith (++)
--         [ (right, [(left, top, p)])
--         | (C.Binary top (Left left) (Right right), p) <- zip rs phi ]
-- 
-- -- | A set of (top, right, potential) tuples for a given terminal.
-- binaryTN :: Nerf (T -> V.Vector (N, N, LogReal))
-- binaryTN = do
--     rs  <- S.toList . C.binary <$> R.asks cfg
--     phi <- mapM phiBinary rs
--     return . onKey . fmap V.fromList $ M.fromListWith (++)
--         [ (left, [(top, right, p)])
--         | (C.Binary top (Right left) (Left right), p) <- zip rs phi ]
-- 
-- -- | A set of (top, potential) tuples for a given left and right terminals.
-- binaryTT :: Nerf (T -> T -> V.Vector (N, LogReal))
-- binaryTT = do
--     rs  <- S.toList . C.binary <$> R.asks cfg
--     phi <- mapM phiBinary rs
--     return . onKey2 . fmap V.fromList $ M.fromListWith (++)
--         [ ((left, right), [(top, p)])
--         | (C.Binary top (Right left) (Right right), p) <- zip rs phi ]
-- 
-- -- | A set of (top, down, unary potential) tuples.
-- unaryN :: Nerf (V.Vector (N, N, LogReal))
-- unaryN = do
--     us  <- S.toList . C.unary <$> R.asks cfg
--     phi <- mapM phiUnary us
--     return $ V.fromList
--         [ (top, down, p)
--         | (C.Unary top (Left down), p) <- zip us phi ]
-- 
-- -- | A set of (top, unary potential) tuples for a given down terminal.
-- unaryT :: Nerf (T -> V.Vector (N, LogReal))
-- unaryT = do
--     us  <- S.toList . C.unary <$> R.asks cfg
--     phi <- mapM phiUnary us
--     return . onKey . fmap V.fromList $ M.fromListWith (++)
--         [ (down, [(top, p)])
--         | (C.Unary top (Right down), p) <- zip us phi ]
-- 
-- onKey :: Ord k => M.Map k (V.Vector b) -> k -> V.Vector b
-- onKey m k = case M.lookup k m of
--     Just v  -> v
--     Nothing -> V.empty
-- 
-- onKey2 :: (Ord a, Ord b) => M.Map (a, b) (V.Vector c) -> a -> b -> V.Vector c
-- onKey2 m k0 k1 = case M.lookup (k0, k1) m of
--     Just v  -> v
--     Nothing -> V.empty
