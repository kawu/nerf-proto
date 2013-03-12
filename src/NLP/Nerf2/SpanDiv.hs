module NLP.Nerf2.SpanDiv
( divTop
) where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import qualified NLP.Nerf2.ListT as L

-- | A set of possible divisions of the (i, j) span into two
-- neighboring (i, k) and (k+1, j) active spans.
-- The result is returned in the ascending order.
divTop :: Pos -> Pos -> L.ListT Nerf Pos
divTop i j = do
    k <- L.liftList [i .. j - 1]
    guard =<< lift (isActive i k)
    guard =<< lift (isActive (k+1) j)
    return k
-- divTop :: Active -> Pos -> Pos -> [Pos]
-- divTop active i j =
--     [ k | k <- [i .. j - 1]
--     , S.member (i, k) active
--     , S.member (k+1, j) active ]
