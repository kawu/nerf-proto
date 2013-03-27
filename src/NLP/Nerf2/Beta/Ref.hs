-- | Putential sums "down", reference functions.

module NLP.Nerf2.Beta.Ref
( beta
, beta'
, beta''
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Tree.Phi
import NLP.Nerf2.Tree.Set
import NLP.Nerf2.Forest.Phi
import NLP.Nerf2.Forest.SubTree
import qualified NLP.Nerf2.Env as Env

beta :: Env.InSent e => e -> N -> Pos -> Pos -> LogReal
beta e x i j
    = sum
        [ phiForest e f / phiSubTree f
        | f <- forestWith e x i j ]
    / fromIntegral (length $ treeSet e (Left x) i j)
  where
    phiSubTree f = case subTree e x i j f of
        Nothing -> error "beta: absurd"
        Just t  -> phiTree e t

beta' :: Env.InSent e => e -> N -> Pos -> Pos -> LogReal
beta' e x i j
    = sum
        [ phiForest e f / phiSubTree f
        | f <- forestWith' e x i j ]
    / fromIntegral (length $ treeSet' e (Left x) i j)
  where
    phiSubTree f = case subTree' e x i j f of
        Nothing -> error "beta': absurd"
        Just t  -> phiTree e t

beta'' :: Env.InSent e => e -> N -> Pos -> Pos -> LogReal
beta'' e x i j
    = sum
        [ phiForest e f / phiSubTree f
        | f <- forestWith'' e x i j ]
    / fromIntegral (length $ treeSet'' e (Left x) i j)
  where
    phiSubTree f = case subTree'' e x i j f of
        Nothing -> error "beta'': absurd"
        Just t  -> phiTree e t
