{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf2.Beta
(
-- * Beta value
  BVal (..)
, at
, at'
-- * Beta map
, Beta
, betaAt
, betaAtM
, computeBeta
) where

import Control.Applicative ((<$>))
import Control.Monad (foldM, forM_, guard, void, when)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Reader as R

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import qualified NLP.Nerf2.Env as Env
import qualified NLP.Nerf2.Active as Active

import NLP.Nerf2.Alpha (Alpha)
import qualified NLP.Nerf2.Alpha as Alpha
import NLP.Nerf2.Gamma (Gamma)
import qualified NLP.Nerf2.Gamma as Gamma
import NLP.Nerf2.Delta (Delta)
import qualified NLP.Nerf2.Delta as Delta

------------------------------------------
-- Beta value and corresponding functions.
------------------------------------------

-- | A beta values corresponding to functions with a hat in
-- the tree model specification.
data BVal = BVal
    { bValR  :: !RVect
    , bVal'' :: !RVect
    , bVal'  :: !RVect }

-- | Beta value at given symbol.
at :: BVal -> N -> LogReal
at BVal{..} (N x)
    = bValR  U.! x
    + bVal'' U.! x
    + bVal'  U.! x
{-# INLINE at #-}

-- | Beta' value at given symbol.
at' :: BVal -> N -> LogReal
at' BVal{..} (N x)
    = bValR  U.! x
    + bVal'' U.! x
{-# INLINE at' #-}

------------------------------------------
-- Beta map.
------------------------------------------

type Beta = M.Map Span BVal

-- | For testing purposes.
betaAtM :: N -> Pos -> Pos -> Nerf LogReal
betaAtM x i j = do
    alpha <- Alpha.computeAlpha
    gamma <- Gamma.computeGamma alpha
    delta <- Delta.computeDelta alpha
    beta  <- computeBeta alpha gamma delta
    return $ betaAt beta x i j

betaAt :: Beta -> N -> Pos -> Pos -> LogReal
betaAt beta x i j =
    case M.lookup (i, j) beta of
        Nothing -> 0
        Just bv -> bv `at` x

-- | Compute beta table given an alpha, gamma and delta tables.
computeBeta :: Alpha -> Gamma -> Delta -> Nerf Beta
computeBeta alpha gamma delta = do
    Env.SentEnv{..} <- R.asks Env.sentEnv
    let update beta (i, j) = do
        v <- betaOn alpha beta gamma delta i j
        return $ M.insert (i, j) v beta
    foldM update M.empty (Active.listDec activeSet)

-- | Compute beta on a given span.
-- Assumption: the span is active.
betaOn :: Alpha -> Beta -> Gamma -> Delta -> Pos -> Pos -> Nerf BVal
betaOn alpha beta gamma delta i j = do
    rR  <- betaOnR  alpha gamma delta i j
    r'' <- betaOn'' alpha beta i j
    r'  <- betaOn'  alpha rR r'' i j
    return $ BVal rR r'' r'

-- betaOnR :: Alpha -> Gamma -> Delta -> Pos -> Pos -> Nerf RVect
-- betaOnR alpha gamma delta i j = do
--     env <- R.ask
--     let lbNum   = Env.inputLength env
--         n       = Env.inputLength env
--         av      = alpha M.! (i, j)
--     return $ runST $ do
--         v <- UM.replicate lbNum 0
--         forM_ (Env.begLabels env) $ \x -> do
--             unsafeAdd v x $ ls x * gt n x
--         markAlpha v av
--         U.unsafeFreeze v
--   where
--     lsGamma = Gamma.ls $ gamma M.! (i - 1)
--     ls x | i > 0     = lsGamma `Gamma.at` x
--          | otherwise = 1
--     gtDelta = Delta.gt $ delta M.! (j + 1)
--     gt n x | j < n - 1 = gtDelta `Delta.at` x
--            | otherwise = 1

betaOnR :: Alpha -> Gamma -> Delta -> Pos -> Pos -> Nerf RVect
betaOnR alpha gamma delta i j = do
    env <- R.ask
    let n  = Env.labelNum env
        av = alpha M.! (i, j)
    return $ runST $ do
        v <- UM.replicate n 0
        forM_ (Env.begLabels env) $ \x -> do
            unsafeAdd v x $ ls x * gt x
        markAlpha v av
        U.unsafeFreeze v
  where
    lsGamma = Gamma.ls <$> M.lookup (i - 1) gamma 
    ls x = maybe 1 (`Gamma.at` x) lsGamma
    gtDelta = Delta.gt <$> M.lookup (j + 1) delta 
    gt x = maybe 1 (`Delta.at` x) gtDelta

betaOn'' :: Alpha -> Beta -> Pos -> Pos -> Nerf RVect
betaOn'' alpha beta i j = do
    lbNum           <- Env.labelNum <$> R.ask
    Env.ParaEnv{..} <- R.asks Env.paraEnv
    Env.SentEnv{..} <- R.asks Env.sentEnv
    let wordAt k    = fst (input V.! k)
        inputLength = V.length input
    return $ runST $ do
        -- Initial vector
        v <- UM.replicate lbNum 0
  
        -- Left child, N->NN rules
        forM_ (Active.divLeft inputLength activeSet i j) $ \k -> do
            let tBV = beta  M.! (i, k)
            let rAV = alpha M.! (j+1, k)
            let phiS = phiNodeMap M.! (i, k)
            forM_ (V.toList binaryNN) $ \(y, x, z, phi) -> do
                unsafeAdd v y $ (tBV `at` x) * phi * (phiS U.! unN x) * (rAV `Alpha.at` z)
  
        -- Left child, N->NT rules
        void $ runMaybeT $ do
            guard $ S.member (j+1, j+1) activeSet
            tBV  <- liftMaybe $ M.lookup (i, j+1) beta
            phiS <- liftMaybe $ M.lookup (i, j+1) phiNodeMap
            let xs = V.toList . binaryNT $ wordAt (j+1)
            lift $ forM_ xs $ \(y, x, phi) -> do
                unsafeAdd v y $ (tBV `at` x) * phi * (phiS U.! unN x)

        -- Right child, N->NN rules
        forM_ (Active.divRight activeSet i j) $ \k -> do
            let lAV = alpha M.! (k, i-1)
            let tBV = beta  M.! (k, j)
            let phiS = phiNodeMap M.! (k, j)
            forM_ (V.toList binaryNN) $ \(y, x, z, phi) -> do
                unsafeAdd v z $ (lAV `Alpha.at` y) * phi * (phiS U.! unN x) * (tBV `at` x)
  
        -- Right child, N->TN rules
        void $ runMaybeT $ do
            guard $ S.member (i-1, i-1) activeSet
            tBV  <- liftMaybe $ M.lookup (i-1, j) beta
            phiS <- liftMaybe $ M.lookup (i-1, j) phiNodeMap
            let xs = V.toList . binaryTN $ wordAt (i-1)
            lift $ forM_ xs $ \(x, z, phi) -> do
                unsafeAdd v z $ (tBV `at` x) * phi * (phiS U.! unN x)
  
        markAlpha v (alpha M.! (i, j))
        U.unsafeFreeze v

betaOn' :: Alpha -> RVect -> RVect -> Pos -> Pos -> Nerf RVect
betaOn' alpha rR r'' i j = do
    lbNum <- Env.labelNum <$> R.ask
    Env.ParaEnv{..} <- R.asks Env.paraEnv
    Env.SentEnv{..} <- R.asks Env.sentEnv
    let phiS        = phiNodeMap M.! (i, j)
    return $ runST $ do
        v <- UM.replicate lbNum 0
        forM_ (V.toList unaryN) $ \(x, y, phi) -> do
            unsafeAdd v y $ (r'' U.! unN x + rR U.! unN x)
                          * phi * (phiS U.! unN x)
        markAlpha'' v (alpha M.! (i, j))
        U.unsafeFreeze v

-- | Zero values in the first vector when alpha value for a corresponding
-- label is equal to 0.
markAlpha :: UM.MVector s LogReal -> Alpha.AVal -> ST s ()
markAlpha v av = do
    let n = UM.length v
    forM_ [0..n-1] $ \i -> do
        when (av `Alpha.at` N i == 0) (UM.write v i 0)
{-# INLINE markAlpha #-}

-- | Zero values in the first vector when alpha value for a corresponding
-- label is equal to 0.
markAlpha'' :: UM.MVector s LogReal -> Alpha.AVal -> ST s ()
markAlpha'' v av = do
    let n = UM.length v
    forM_ [0..n-1] $ \i -> do
        when (av `Alpha.atB` N i == 0) (UM.write v i 0)
{-# INLINE markAlpha'' #-}

-- | TODO: Make it really unsafe.
unsafeAdd :: (Num a, U.Unbox a) => UM.MVector s a -> N -> a -> ST s ()
unsafeAdd v (N i) y = do
    x <- UM.read v i
    UM.write v i (x + y)
{-# INLINE unsafeAdd #-}

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
