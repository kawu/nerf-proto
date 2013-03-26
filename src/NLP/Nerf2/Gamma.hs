{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf2.Gamma
( Gamma
, GVal (..)
, BsM
, EqM
, LsM
, at
, bsAt
, eqAt
, lsAt
, computeGamma
-- * Testing
, bsAtM
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

-- | Ls values for a given position.
type LsM = M.Map N LogReal

at :: M.Map N LogReal -> N -> LogReal
at m x = M.findWithDefault 0 x m

-- | All values for a given position.
data GVal = GVal
    { bs  :: BsM
    , eq  :: EqM
    , ls  :: LsM }
    deriving (Show, Eq, Ord)

-- | A gamma map.
type Gamma = M.Map Pos GVal

bsAt :: Gamma -> N -> Pos -> LogReal
bsAt m x i = maybe 0 id $ do
    m' <- bs <$> M.lookup i m
    M.lookup x m'

eqAt :: Gamma -> N -> Pos -> LogReal
eqAt m x i = maybe 0 id $ do
    m' <- eq <$> M.lookup i m
    M.lookup x m'

lsAt :: Gamma -> N -> Pos -> LogReal
lsAt m x i
    | i < 0     = 1
    | otherwise = maybe 0 id $ do
        m' <- ls <$> M.lookup i m
        M.lookup x m'

computeGamma :: A.Alpha -> Nerf Gamma
computeGamma alpha = do
    env <- R.ask
    let update m i = do
        gv <- gammaAll alpha m i
        return $ M.insert i gv m
    foldM update M.empty 
        [i | i <- [0 .. Env.inputLength env - 1]]

gammaAll :: A.Alpha -> Gamma -> Pos -> Nerf GVal
gammaAll a m i = do
    eq <- gammaEq a m i
    bs <- gammaBs m eq i
    ls <- gammaLs bs
    return $ GVal
        { bs = bs
        , eq = eq
        , ls = ls }

-- | Make a map from starting symbols to values determined by
-- the given function. 
mkSMap :: (N -> LogReal) -> Nerf (M.Map N LogReal)
mkSMap f = do
    env <- R.ask
    return $ M.fromList [(x, f x) | x <- Env.begLabels env]

gammaBs :: Gamma -> EqM -> Pos -> Nerf BsM
gammaBs m eq i = mkSMap $ \x ->
    bsAt m x (i-1) + eq `at` x

gammaEq :: A.Alpha -> Gamma -> Pos -> Nerf EqM
gammaEq a m i = do
    env <- R.ask
    mkSMap $ \x -> sum
        [ lsAt m x (k-1) * A.alphaAt a x k i
        | k <- [0 .. i], Env.isActive env k i ]

gammaLs :: BsM -> Nerf LsM
gammaLs bs = do
    env <- R.ask
    mkSMap $ \x -> sum
        [ bs `at` y * Env.phiEdge env y x
        | y <- Env.begLabels env ]

-- | Testing function.
bsAtM :: N -> Pos -> Nerf LogReal
bsAtM x i = do
    alpha <- A.computeAlpha
    gamma <- computeGamma alpha
    return $ bsAt gamma x i
