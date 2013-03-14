{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Computations in logarithmic scale.

module NLP.Nerf2.LogReal
( LogReal
, fromLogReal
, logToLogReal
, logFromLogReal
) where

import Prelude hiding (sum, product)
import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Binary (Binary)
import qualified Data.Vector.Unboxed as U

newtype LogReal = LogReal Double deriving
    ( Show, Read, Eq, Ord, Binary
    , Vector U.Vector, MVector U.MVector, U.Unbox )

foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double

zero :: Double
zero = -(1/0)

isZero :: Double -> Bool
isZero = (zero==)

instance Num LogReal where
    LogReal x * LogReal y = LogReal $ x + y
    LogReal x + LogReal y
        | isZero x  = LogReal $ y
        | x > y     = LogReal $ x + log1p(exp(y - x))
        | otherwise = LogReal $ y + log1p(exp(x - y))
    LogReal _ - LogReal _ = error "LogReal: (-) not supported"
    negate _    = error "LogReal: negate not supported"
    abs         = id
    signum (LogReal x)
        | x > zero  = 1
        | otherwise = 0
    fromInteger x
        | x == 0    = LogReal zero
        | x > 0     = LogReal . log . fromInteger $ x
        | otherwise = error "LogReal: fromInteger on negative argument"

instance Fractional LogReal where
    LogReal x / LogReal y = LogReal $ x - y
    fromRational x
        | x == 0    = LogReal zero
        | x > 0     = LogReal . log . fromRational $ x
        | otherwise = error "LogReal: fromRational on negative argument"
-- This is simply a polymorphic version of the 'LogReal' data
-- constructor. We present it mainly because we hide the constructor
-- in order to make the type a bit more opaque. If the polymorphism
-- turns out to be a performance liability because the rewrite rules
-- can't remove it, then we need to rethink all four
-- constructors\/destructors.
--
-- | Constructor which assumes the argument is already in the
-- log-domain.
logToLogReal :: Double -> LogReal
logToLogReal = LogReal

-- | Return our log-domain value back into normal-domain.
fromLogReal :: LogReal -> Double
fromLogReal (LogReal x) = exp x

-- | Return the log-domain value itself without conversion.
logFromLogReal :: LogReal -> Double
logFromLogReal (LogReal x) = x
