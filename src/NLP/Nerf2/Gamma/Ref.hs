{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf2.Gamma.Ref
( gamma
, gamma'
) where

import Data.List (foldl')

import NLP.Nerf2.Types
import NLP.Nerf2.Forest.Set
import NLP.Nerf2.Forest.Phi
import qualified NLP.Nerf2.Env as Env

gamma :: Env.InSent e => e -> N -> Pos -> LogReal
gamma env x i = sumForests env $ forestSetF env x i

gamma' :: Env.InSent e => e -> N -> Pos -> LogReal
gamma' env x i = sumForests env $ forestSetF' env x i

sumForests :: Env.InSent e => e -> [Forest] -> LogReal
sumForests env = foldl' (+) 0 . map (phiForest env)
