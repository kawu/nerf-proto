{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

-- | An internal, monadic interface.

module NLP.Nerf2.Monad
( Nerf
, runNerf
, activeCond
) where

import qualified Control.Monad.Reader as R

import NLP.Nerf2.Types
import qualified NLP.Nerf2.Env as Env

-- | A Nerf monad.
type Nerf = R.Reader Env.Layer2

-- | Run the `Nerf` monad.
runNerf :: Env.Layer2 -> Nerf a -> a
runNerf = flip R.runReader

-- | Conditional execution.
activeCond :: Pos -> Pos -> Nerf a -> Nerf a -> Nerf a
activeCond i j n m = do
    isActive <- R.asks Env.isActive
    if isActive i j then m else n
