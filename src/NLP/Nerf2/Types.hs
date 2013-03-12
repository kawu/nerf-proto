{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Basic Nerf internal types.

module NLP.Nerf2.Types
( LogReal
, O (..)
, N (..)
, T (..)
, Sent
, Pos
, mkO
, mkN
, mkT
) where

import Data.Int

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Number.LogFloat as L

import Data.Binary (Binary)
import Data.Vector.Binary ()

-- | A real value.
type LogReal = L.LogFloat

-- | An observation.
newtype O = O { unO :: Int32 }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A non-terminal symbol.
newtype N = N { unN :: Int16 }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A terminal symbol.
newtype T = T { unT :: Int16 }
    deriving ( Show, Read, Eq, Ord, Binary
             , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | An input sentence consists of a list of words.  For each word
-- a terminal symbol and a set of observations is given.
type Sent = V.Vector (T, U.Vector O) 

-- | A type synonym for a position in the sentence.
type Pos = Int

mkO :: Integral a => a -> O
mkO = O . fromIntegral
{-# INLINE mkO #-}

mkN :: Integral a => a -> N
mkN = N . fromIntegral
{-# INLINE mkN #-}

mkT :: Integral a => a -> T
mkT = T . fromIntegral
{-# INLINE mkT #-}
