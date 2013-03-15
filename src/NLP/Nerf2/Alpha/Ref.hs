-- | Putential sums "up", reference functions.

module NLP.Nerf2.Alpha.Ref
( alpha
, alpha'
, alpha''
) where

import Data.List (foldl')
import qualified Control.Monad.Reader as R

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import NLP.Nerf2.Tree
import NLP.Nerf2.Tree.Set
import NLP.Nerf2.Tree.Phi
import qualified NLP.Nerf2.Env as Env

alpha :: Env.InSent e => e -> Either N T -> Pos -> Pos -> LogReal
alpha env x i j =  sumTrees env $ treeSet env x i j

alpha' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> LogReal
alpha' env x i j = sumTrees env $ treeSet' env x i j

alpha'' :: Env.InSent e => e -> Either N T -> Pos -> Pos -> LogReal
alpha'' env x i j = sumTrees env $ treeSet'' env x i j

sumTrees :: Env.InSent e => e -> [Tree] -> LogReal
sumTrees env = foldl' (+) 0 . map (phiTree env)
