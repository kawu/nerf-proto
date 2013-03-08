{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Basic Nerf internal types.

module NLP.Nerf.Types
( O (..)
, N (..)
, T (..)
) where

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as U

import Data.Binary (Binary)
import Data.Vector.Binary ()

-- | An observation.
newtype O = O { unO :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A non-terminal symbol.
newtype N = N { unN :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A terminal symbol.
newtype T = T { unT :: Int }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )
