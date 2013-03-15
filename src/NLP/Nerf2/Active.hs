-- | A set of active range values.

module NLP.Nerf2.Active
( Active
, listInc
, listDec
, divTop
, divTopE
) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as S

import NLP.Nerf2.Types
import qualified NLP.Nerf2.Env as E

-- | List all active spans in order of increasing sizes.
-- TODO: This function is a stub.
listInc :: Active -> [Span]
listInc =
    let size (i, j) = j - i
    in  sortBy (comparing size) . S.toList

-- | List all active spans in order of decreasing sizes.
listDec :: Active -> [Span]
listDec = undefined

-- | A set of possible divisions of the (i, j) span into two
-- neighboring (i, k) and (k+1, j) active spans.
-- The result is returned in the ascending order.
-- Pure version.
divTop :: Active -> Pos -> Pos -> [Pos]
divTop active i j =
    [ k | k <- [i .. j - 1]
    , S.member (i, k) active
    , S.member (k+1, j) active ]

-- | Wrapper for `divTop`.
divTopE :: E.InSent e => e -> Pos -> Pos -> [Pos]
divTopE = divTop . E.activeSet . E.sentEnv
