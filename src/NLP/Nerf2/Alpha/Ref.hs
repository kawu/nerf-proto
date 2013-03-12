-- | Putential sums "up", reference functions.

module NLP.Nerf2.Alpha.Ref
( alpha
, alpha'
, alpha''
) where

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import NLP.Nerf2.Tree
import NLP.Nerf2.Tree.Set
import NLP.Nerf2.Tree.Phi
import qualified NLP.Nerf2.ListT as L

alpha :: Either N T -> Pos -> Pos -> Nerf LogReal
alpha x i j = sumTrees $ treeSet x i j

alpha' :: Either N T -> Pos -> Pos -> Nerf LogReal
alpha' x i j = sumTrees $ treeSet' x i j

alpha'' :: Either N T -> Pos -> Pos -> Nerf LogReal
alpha'' x i j = sumTrees $ treeSet'' x i j

sumTrees :: L.ListT Nerf Tree -> Nerf LogReal
sumTrees =
    L.foldListT plus (return 0)
  where
    plus t my = do
        x <- phiTree t
        y <- my
        return $ x + y
