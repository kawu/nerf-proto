module NLP.Nerf2.SpanDiv
( divTop
) where

import qualified Data.Set as S

import NLP.Nerf2.Types
import NLP.Nerf2.Active

-- | A set of possible divisions of the (i, j) span into two
-- neighboring (i, k) and (k+1, j) active spans.
-- The result is returned in the ascending order.
divTop :: Active -> Pos -> Pos -> [Pos]
divTop active i j =
    [ k | k <- [i .. j - 1]
    , S.member (i, k) active
    , S.member (k+1, j) active ]
