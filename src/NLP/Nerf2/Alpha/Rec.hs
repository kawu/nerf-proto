-- | Putential sums "up", recursive definitions.

module NLP.Nerf2.Alpha.Rec
( alpha
, alpha'
, alpha''
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import NLP.Nerf2.SpanDiv
import qualified NLP.Nerf2.CFG as C
import qualified NLP.Nerf2.ListT as L

alpha :: Either N T -> Pos -> Pos -> Nerf LogReal
alpha n i j = (+) <$> alpha' n i j <*> alpha'' n i j

alpha' :: Either N T -> Pos -> Pos -> Nerf LogReal
alpha' (Left x) i j = do
    cfg <- nerfCFG
    (*) <$> phiNode x i j <*> foldM plus 0
        [ (*) <$> phiUnary u <*> alpha'' (C.down u) i j
        | u <- C.perTopU cfg x ]
alpha' (Right _) _ _ = return 0

alpha'' :: Either N T -> Pos -> Pos -> Nerf LogReal
alpha'' (Left x) i j
    | i == j    = return 0
    | otherwise = do
        cfg <- nerfCFG
        (*) <$> phiNode x i j <*> do
            L.foldListT plus (return 0) $ do
                r   <- L.liftList $ C.perTopB cfg x
                a2  <- lift $ phiBinary r
                k   <- divTop i j
                -- TODO: ListT needs Applicative instance to simplify
                -- the process below.
                a3  <- product <$> sequence
                    [ lift (alpha (C.left r)  i k)
                    , lift (alpha (C.right r) (k+1) j) ]
                return (a2 * a3)
alpha'' (Right x) i j
    | i == j    = inputHas i x >>= \b -> return $
        if b then 1 else 0
    | otherwise = return 0

plus :: (Functor m, Num a) => a -> m a -> m a
plus y mz = (y+) <$> mz
{-# INLINE plus #-}
