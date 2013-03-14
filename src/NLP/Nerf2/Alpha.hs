module NLP.Nerf2.Alpha
( Alpha
, AVal (..)
, computeAlpha
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, forM_, guard, void)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Map.Strict as M

import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
-- import qualified Control.Monad.ST as ST

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import qualified NLP.Nerf2.Active as A
import qualified NLP.Nerf2.CFG as C

-- | An alpha value.
data AVal = AVal
    { avalF :: !RVect
    , avalB :: !RVect }

-- | Alpha value at given symbol.
at :: AVal -> N -> LogReal
at v x = atF v x + atB v x

-- | Alpha' value at given symbol.
atF :: AVal -> N -> LogReal
atF (AVal v _) (N x) = v U.! x

-- | Alpha'' value at given symbol.
atB :: AVal -> N -> LogReal
atB (AVal _ w) (N x) = w U.! x

-- | An `RVect` with 0 values.
rvZero :: Nerf RVect
rvZero = flip U.replicate 0 <$> labelNum

-- | An `AVal` with 0 values.
avalZero :: Nerf AVal
avalZero = AVal <$> rvZero <*> rvZero

type Alpha = M.Map Span AVal

computeAlpha :: Nerf Alpha
computeAlpha = do
    act <- activeSet
    let update m s = do
        v <- alpha m s
        return $ M.insert s v m
    foldM update M.empty (A.listInc act)

-- | A map from spans to vectors of phiNode values.
type SPhi = M.Map Span RVect

-- computeSPhi :: 

alpha :: Alpha -> Span -> Nerf AVal
alpha = undefined

-- alpha :: Alpha -> Span -> Nerf AVal
-- alpha alphaMap (i, j) = do
--     act     <- activeSet
--     xsV     <- labelVect
--     phiB    <- perTopB'NN
--     phiS    <- phiNodeMap
-- 
--     let n   = U.length xsV
--     runST $ do
--         -- First compute fork alpha values.
--         aF  <- UM.new n
        
alpha'' :: Alpha -> Pos -> Pos -> Nerf RVect
alpha'' m i j = activeCond i j rvZero $ alphaI'' m i j

alphaI'' :: Alpha -> Pos -> Pos -> Nerf RVect
alphaI'' alphaMap i j
    | i == j    = rvZero
    | otherwise = do
        act     <- activeSet
        xsV     <- labelVect

        phiS    <- (M.! (i,j)) <$> phiNodeMap
        binNN   <- binaryNN
        binNT   <- binaryNT
        binTN   <- binaryTN
        binTT   <- binaryTT

        sent    <- input
        let wordAt i = fst (sent V.! i)
        
        return $ runST $ do
          -- Initial vector
          v <- UM.replicate (U.length xsV) 0

          -- Compute base values
          forM_ (A.divTop' act i j) $ \k -> do
            let lAV = alphaMap M.! (i, k)
            let rAV = alphaMap M.! (k+1, j)
            forM_ (V.toList binNN) $ \(y, x, z, phi) -> do
              unsafeAdd v (unN x) $ (lAV `at` y) * phi * (rAV `at` z)

          -- Consider N->TN binary rules
          void $ runMaybeT $ do
            rAV <- liftMaybe $ M.lookup (i+1, j) alphaMap
            let xs = V.toList . binTN $ wordAt i
            lift $ forM_ xs $ \(x, z, phi) -> do
              unsafeAdd v (unN x) $ phi * (rAV `at` z)

          -- Consider N->NT binary rules
          void $ runMaybeT $ do
            lAV <- liftMaybe $ M.lookup (i, j-1) alphaMap
            let xs = V.toList . binNT $ wordAt j
            lift $ forM_ xs $ \(y, x, phi) -> do
              unsafeAdd v (unN x) $ (lAV `at` y) * phi

          -- Consider N->TT binary rules
          void $ runMaybeT $ do
            guard $ j == i + 1
            let xs = V.toList $ binTT (wordAt i) (wordAt j)
            lift $ forM_ xs $ \(x, phi) -> do
              unsafeAdd v (unN x) phi

          -- Update with respect to phiNode values
          mulByV v phiS

          U.unsafeFreeze v

-- | Multiply two vectors value after value and store the result
-- in the first vector.
mulByV :: (Num a, U.Unbox a) => UM.MVector s a -> U.Vector a -> ST s ()
mulByV v w = do
    let n = min (UM.length v) (U.length w)
    forM_ [0..n-1] $ \i -> do
        unsafeAdd v i (w U.! i)
{-# INLINE mulByV #-}

-- | TODO: Make it really unsafe.
unsafeAdd :: (Num a, U.Unbox a) => UM.MVector s a -> Int -> a -> ST s ()
unsafeAdd v i y = do
    x <- UM.read v i
    UM.write v i (x + y)
{-# INLINE unsafeAdd #-}

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
