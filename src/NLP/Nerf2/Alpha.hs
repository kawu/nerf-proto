module NLP.Nerf2.Alpha
( Alpha
, alphaAt
, AVal (..)
, at
, atF
, atB
, computeAlpha
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, forM_, guard, void)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
-- import qualified Control.Monad.ST as ST

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import qualified NLP.Nerf2.Active as A

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
avZero :: Nerf AVal
avZero = AVal <$> rvZero <*> rvZero

type Alpha = M.Map Span AVal

-- | For testing purposes.
alphaAt :: N -> Pos -> Pos -> Nerf LogReal
alphaAt x i j = do
    alp <- computeAlpha
    return $ case M.lookup (i, j) alp of
        Nothing -> 0
        Just av -> av `at` x

computeAlpha :: Nerf Alpha
computeAlpha = do
    act <- activeSet
    let update m (i, j) = do
        v <- alpha m i j
        return $ M.insert (i, j) v m
    foldM update M.empty (A.listInc act)

-- | A map from spans to vectors of phiNode values.
-- type SPhi = M.Map Span RVect
-- computeSPhi :: 

alpha :: Alpha -> Pos -> Pos -> Nerf AVal
alpha m i j = activeCond i j avZero $ do
    r'' <- alphaI'' m i j
    r'  <- alphaI' r'' i j
    return (AVal r' r'')

-- | Vector of alpha' values given vector of alpha'' values.
alphaI' :: RVect -> Pos -> Pos -> Nerf RVect
alphaI' a'' i j = do
    lbNum   <- labelNum
    -- phiS    <- (M.! (i,j)) <$> phiNodeMap
    phiS    <- look "A" (i,j) <$> phiNodeMap
    unaN    <- unaryN
    unaT    <- unaryT
    sent    <- input
    let wordAt k = fst (sent V.! k)
    return $ runST $ do
      -- Initial vector
      v <- UM.replicate lbNum 0

      if (i == j)
        then do -- N->T rules
          let xs = V.toList $ unaT (wordAt i)
          forM_ xs $ \(x, phi) -> do
            unsafeAdd v x phi
        else do -- N->N rules
          forM_ (V.toList unaN) $ \(x, y, phi) -> do
            unsafeAdd v x $ phi * (a'' U.! unN y)
      
      -- Update with respect to phiNode values
      mulByV v phiS

      U.unsafeFreeze v

-- | Vector of alpha'' values.
alphaI'' :: Alpha -> Pos -> Pos -> Nerf RVect
alphaI'' alphaMap i j
    | i == j    = rvZero
    | otherwise = do
        act     <- activeSet
        lbNum   <- labelNum

        -- phiS    <- (M.! (i,j)) <$> phiNodeMap
        phiS    <- look "B" (i,j) <$> phiNodeMap
        binNN   <- binaryNN
        binNT   <- binaryNT
        binTN   <- binaryTN
        binTT   <- binaryTT

        sent    <- input
        let wordAt k = fst (sent V.! k)
        
        return $ runST $ do
          -- Initial vector
          v <- UM.replicate lbNum 0

          -- N->NN rules
          forM_ (A.divTop' act i j) $ \k -> do
            -- let lAV = alphaMap M.! (i, k)
            let lAV = look "C" (i, k) alphaMap
            -- let rAV = alphaMap M.! (k+1, j)
            let rAV = look "D" (k+1, j) alphaMap
            forM_ (V.toList binNN) $ \(y, x, z, phi) -> do
              unsafeAdd v x $ (lAV `at` y) * phi * (rAV `at` z)

          -- N->TN rules
          void $ runMaybeT $ do
            guard $ S.member (i, i) act
            rAV <- liftMaybe $ M.lookup (i+1, j) alphaMap
            let xs = V.toList . binTN $ wordAt i
            lift $ forM_ xs $ \(x, z, phi) -> do
              unsafeAdd v x $ phi * (rAV `at` z)

          -- N->NT rules
          void $ runMaybeT $ do
            guard $ S.member (j, j) act
            lAV <- liftMaybe $ M.lookup (i, j-1) alphaMap
            let xs = V.toList . binNT $ wordAt j
            lift $ forM_ xs $ \(y, x, phi) -> do
              unsafeAdd v x $ (lAV `at` y) * phi

          -- N->TT rules
          void $ runMaybeT $ do
            guard $ j == i + 1
            guard $ S.member (i, i) act
            guard $ S.member (j, j) act
            let xs = V.toList $ binTT (wordAt i) (wordAt j)
            lift $ forM_ xs $ \(x, phi) -> do
              unsafeAdd v x phi

          -- Update with respect to phiNode values
          mulByV v phiS

          U.unsafeFreeze v

-- | Multiply two vectors value after value and store the result
-- in the first vector.
mulByV :: (Num a, U.Unbox a) => UM.MVector s a -> U.Vector a -> ST s ()
mulByV v w = do
    let n = min (UM.length v) (U.length w)
    forM_ [0..n-1] $ \i -> do
        unsafeMul v i (w U.! i)
{-# INLINE mulByV #-}

-- | TODO: Make it really unsafe.
unsafeAdd :: (Num a, U.Unbox a) => UM.MVector s a -> N -> a -> ST s ()
unsafeAdd v (N i) y = do
    x <- UM.read v i
    UM.write v i (x + y)
{-# INLINE unsafeAdd #-}

-- | TODO: Make it really unsafe.
unsafeMul :: (Num a, U.Unbox a) => UM.MVector s a -> Int -> a -> ST s ()
unsafeMul v i y = do
    x <- UM.read v i
    UM.write v i (x * y)
{-# INLINE unsafeMul #-}

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

look :: Ord a => String -> a -> M.Map a b -> b
look err k m = case M.lookup k m of
    Nothing -> error $ "look: " ++ err
    Just v  -> v
