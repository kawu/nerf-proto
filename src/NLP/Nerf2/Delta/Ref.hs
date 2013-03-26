{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf2.Delta.Ref
( delta
, delta'
) where

import Data.List (foldl')

import NLP.Nerf2.Types
import NLP.Nerf2.Forest.Set
import NLP.Nerf2.Forest.Phi
import qualified NLP.Nerf2.Env as Env

delta :: Env.InSent e => e -> N -> Pos -> LogReal
delta env x i = sumForests env $ forestSetB env x i

delta' :: Env.InSent e => e -> N -> Pos -> LogReal
delta' env x i = sumForests env $ forestSetB' env x i

sumForests :: Env.InSent e => e -> [Forest] -> LogReal
sumForests env = foldl' (+) 0 . map (phiForest env)
