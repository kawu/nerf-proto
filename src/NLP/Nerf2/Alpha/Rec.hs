-- | Putential sums "up", recursive definitions.

module NLP.Nerf2.Alpha.Rec
( alpha
, alpha'
, alpha''
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Active
import qualified NLP.Nerf2.CFG as C
import qualified NLP.Nerf2.Env as Env

alpha :: Env.InSent e => e -> Either N T -> Pos -> Pos -> LogReal
alpha env n i j = alpha' env n i j + alpha'' env n i j

alpha' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> LogReal
alpha' env n i j = if Env.isActive (Env.sentEnv env) i j
    then alphaI' env n i j
    else 0

alphaI' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> LogReal
alphaI' env (Left x) i j
    = Env.phiNode (Env.paraEnv env) x i j
    * sum [ Env.phiUnary (Env.paraEnv env) u *
            alpha'' env (C.down u) i j
          | u <- C.perTopU (Env.cfg $ Env.mainEnv env) x ]
alphaI' _ (Right _) _ _ = 0

alpha'' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> LogReal
alpha'' env n i j = if Env.isActive (Env.sentEnv env) i j
    then alphaI'' env n i j
    else 0

alphaI'' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> LogReal
alphaI'' env (Left x) i j
    | i == j    = 0
    | otherwise
        = Env.phiNode (Env.paraEnv env) x i j
        * sum [ phi * alpha env (C.left r)  i k
                    * alpha env (C.right r) (k+1) j
              | r <- C.perTopB (Env.cfg $ Env.mainEnv env) x
              , let phi = Env.phiBinary (Env.paraEnv env) r
              , k <- divTop' (Env.activeSet $ Env.sentEnv env) i j ]
alphaI'' env (Right x) i j
    | i == j    = 
        let fromBool b = if b then 1 else 0
        in  fromBool $ Env.inputHas (Env.sentEnv env) i x
    | otherwise = 0
