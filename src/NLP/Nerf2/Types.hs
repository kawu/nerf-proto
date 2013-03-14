{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Basic Nerf internal types.

module NLP.Nerf2.Types
( O (..)
, N (..)
, T (..)
, Sent
, Pos
, Span
, Active
, mkO
, mkN
, mkT
, L.LogReal
, RVect
) where

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.Binary (Binary)
import Data.Vector.Binary ()

import qualified NLP.Nerf2.LogReal as L

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

-- | An input sentence consists of a list of words.  For each word
-- a terminal symbol and a set of observations is given.
type Sent = V.Vector (T, U.Vector O) 

-- | A type synonym for a position in the sentence.
type Pos = Int

-- | A span.
type Span = (Pos, Pos)

-- | A set of active range values.  For each (i, j) span,
-- which is not in the active set, the set of potential
-- trees is set to be empty.
type Active = S.Set Span

mkO :: Integral a => a -> O
mkO = O . fromIntegral
{-# INLINE mkO #-}

mkN :: Integral a => a -> N
mkN = N . fromIntegral
{-# INLINE mkN #-}

mkT :: Integral a => a -> T
mkT = T . fromIntegral
{-# INLINE mkT #-}

-- | Real-valued vector.
type RVect = U.Vector L.LogReal
