-- | A set of active range values.

module NLP.Nerf2.Active
( Active
) where

import qualified Data.Set as S

import NLP.Nerf2.Types (Pos)

-- | A set of active range values.  For each (i, j) span,
-- which is not in the active set, the set of potential
-- trees is set to be empty.
type Active = S.Set (Pos, Pos)
