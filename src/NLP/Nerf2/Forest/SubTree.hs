{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module NLP.Nerf2.Forest.SubTree
(
-- * Subtree
  subTree
, subTree'
, subTree''
-- * Forest with subtree
, forestWith
, forestWith'
, forestWith''
) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad (msum, guard)
import Data.Maybe (isJust)

import NLP.Nerf2.Types
import NLP.Nerf2.Tree
import NLP.Nerf2.Forest.Set
import qualified NLP.Nerf2.Env as Env

-- | Any subtree of a forest on a given span.
subTree :: Env.InSent e => e -> N -> Pos -> Pos -> Forest -> Maybe Tree
subTree e x i j ts = msum [subTreeT e x i j t | t <- ts]

-- | `Branch` subtree of a forest on a given span.
subTree' :: Env.InSent e => e -> N -> Pos -> Pos -> Forest -> Maybe Tree
subTree' e x i j ts = do
    t <- subTree e x i j ts
    guard $ isBranch t
    return t

-- | `Fork` subtree of a forest on a given span.
subTree'' :: Env.InSent e => e -> N -> Pos -> Pos -> Forest -> Maybe Tree
subTree'' e x i j ts = do
    t <- subTree e x i j ts
    guard $ isFork t
    return t

-- | Any subtree of a tree spanned over the given range.
subTreeT :: Env.InSent e => e -> N -> Pos -> Pos -> Tree -> Maybe Tree
subTreeT e x i j t = unPosify <$> subTreeT' e x i j (posify t)

subTreeT' :: Env.InSent e => e -> N -> Pos -> Pos -> TreeP -> Maybe TreeP
subTreeT' e x i j t = do
    guard $ begP t <= i && j <= endP t
    here <|> lower
  where
    here = do
        sp <- over t
        guard $ sp == (x, i, j)
        return t
    lower = msum . map (subTreeT' e x i j) $ children t

    over u = (, begP u, endP u) <$> symP u
    symP LeafP{..} = Nothing
    symP u         = Just (labelP u)

    children ForkP{..}   = [leftP, rightP]
    children BranchP{..} = [downP]
    children _           = []

-- | A set of forests with a subtree spanned over the given range and symbol.
forestWith :: Env.InSent e => e -> N -> Pos -> Pos -> [Forest]
forestWith e x i j = filter (isJust . subTree e x i j) (forestSet e)

-- | A set of forests with a 'Branch' spanned over the given range and symbol.
forestWith' :: Env.InSent e => e -> N -> Pos -> Pos -> [Forest]
forestWith' e x i j = filter (isJust . subTree' e x i j) (forestSet e)

-- | A set of forests with a 'Fork' spanned over the given range and symbol.
forestWith'' :: Env.InSent e => e -> N -> Pos -> Pos -> [Forest]
forestWith'' e x i j = filter (isJust . subTree'' e x i j) (forestSet e)
