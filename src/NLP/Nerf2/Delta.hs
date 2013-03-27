{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf2.Delta
( Delta
, DVal (..)
, BsM
, EqM
, GtM
, at
, bsAt
, eqAt
, gtAt
, computeDelta
, norm
-- * Testing
, bsAtM
, normTest
) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import qualified Data.Map.Strict as M

import qualified Control.Monad.Reader as R

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import qualified NLP.Nerf2.Alpha as A
import qualified NLP.Nerf2.Env as Env

-- | Base values for a given position.
type BsM = M.Map N LogReal

-- | Eq values for a given position.
type EqM = M.Map N LogReal

-- | Gt values for a given position.
type GtM = M.Map N LogReal

-- | Just a lookup with default 0 value.
at :: M.Map N LogReal -> N -> LogReal
at m x = M.findWithDefault 0 x m

-- | All values for a given position.
data DVal = DVal
    { bs  :: BsM
    , eq  :: EqM
    , gt  :: GtM }
    deriving (Show, Eq, Ord)

-- | A delta map.
type Delta = M.Map Pos DVal

bsAt :: Delta -> N -> Pos -> LogReal
bsAt m x i = maybe 0 id $ do
    m' <- bs <$> M.lookup i m
    M.lookup x m'

eqAt :: Delta -> N -> Pos -> LogReal
eqAt m x i = maybe 0 id $ do
    m' <- eq <$> M.lookup i m
    M.lookup x m'

gtAt :: Delta -> N -> Pos -> LogReal
gtAt m x i
    | i < 0     = 0     -- Is it reduntant?
    | otherwise = case M.lookup i m of
        Nothing -> 1
        Just m' -> gt m' `at` x

computeDelta :: A.Alpha -> Nerf Delta
computeDelta alpha = do
    env <- R.ask
    let update m i = do
        dv <- deltaAll alpha m i
        return $ M.insert i dv m
    let down i = [i, i-1 .. 0]
    foldM update M.empty 
        [i | i <- down (Env.inputLength env - 1)]

deltaAll :: A.Alpha -> Delta -> Pos -> Nerf DVal
deltaAll a m i = do
    eq <- deltaEq a m i
    bs <- deltaBs m eq i
    gt <- deltaGt bs
    return $ DVal
        { bs = bs
        , eq = eq
        , gt = gt }

-- | Make a map from starting symbols to values determined by
-- the given function. 
mkSMap :: (N -> LogReal) -> Nerf (M.Map N LogReal)
mkSMap f = do
    env <- R.ask
    return $ M.fromList [(x, f x) | x <- Env.begLabels env]

deltaBs :: Delta -> EqM -> Pos -> Nerf BsM
deltaBs m eq i = mkSMap $ \x ->
    bsAt m x (i+1) + eq `at` x

deltaEq :: A.Alpha -> Delta -> Pos -> Nerf EqM
deltaEq a m i = do
    env <- R.ask
    mkSMap $ \x -> sum
        [ A.alphaAt a x i k * gtAt m x (k+1)
        | k <- [i .. Env.inputLength env - 1]
        , Env.isActive env i k ]

deltaGt :: BsM -> Nerf GtM
deltaGt bs = do
    env <- R.ask
    mkSMap $ \x -> 1 + sum
        [ bs `at` y * Env.phiEdge env x y
        | y <- Env.begLabels env ]

-- | Testing function.
bsAtM :: N -> Pos -> Nerf LogReal
bsAtM x i = do
    alpha <- A.computeAlpha
    delta <- computeDelta alpha
    return $ bsAt delta x i

-- | Normalization factor.
norm :: Delta -> Nerf LogReal
norm delta = do
    env <- R.ask
    return $ 1 + sum
        [ bsAt delta x 0
        | x <- Env.begLabels env ]

-- | Noramlization factor, testing function.
normTest :: Nerf LogReal
normTest = norm =<< computeDelta =<< A.computeAlpha
