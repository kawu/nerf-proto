{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

import Control.Applicative ((<$>), (<*>), pure)
import System.Exit (exitFailure)
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Number.LogFloat as L

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import qualified NLP.Nerf2.CFG as CFG
import qualified NLP.Nerf2.Alpha.Ref as AF
import qualified NLP.Nerf2.Alpha.Rec as AC

-- | QuickCheck parameters.
posMax      = 5
tMax        = 3
nMax        = 3
unaryMax    = 10
binaryMax   = 10
phiMax      = 10.0 :: Double
descMax     = 10
activeMax   = 25
featMax     = 10

-- | Arbitrary set of maximum k elements (minimum 1?).
arbitrarySet :: Ord a => Int -> Gen a -> Gen (S.Set a)
arbitrarySet k g = S.fromList <$> vectorOf k g

-- | Arbitrary map of maximum k elements (minimum 1?).
arbitraryMap :: Ord a => Int -> Gen a -> Gen b -> Gen (M.Map a b)
arbitraryMap k g g' = (M.fromList.) . zip
    <$> vectorOf k g
    <*> vectorOf k g'

arbitraryPos :: Gen Pos
arbitraryPos = choose (1, posMax)

arbitrarySpan :: Gen (Pos, Pos)
arbitrarySpan =
    let pair = (,) <$> arbitraryPos <*> arbitraryPos
    in  pair `suchThat` \(i, j) -> i <= j

arbitraryReal :: Gen LogReal
arbitraryReal = L.logToLogFloat <$> choose (-phiMax, phiMax)

-- | Arbitrary terminal.
arbitraryT :: Gen T 
arbitraryT = T <$> choose (1, tMax)

-- | Arbitrary nonterminal.
arbitraryN :: Gen N 
arbitraryN = N <$> choose (1, nMax)

arbitraryEither :: Gen a -> Gen b -> Gen (Either a b)
arbitraryEither g g' = arbitrary >>= \b -> case b of
    False   -> Left <$> g
    True    -> Right <$> g'

-- | Arbitrary nonterminal or terminal.
arbitraryNT :: Gen (Either N T)
arbitraryNT = arbitraryEither arbitraryN arbitraryT

-- | Arbitrary "node rule".
arbitraryNode :: Gen (N, Pos, Pos)
arbitraryNode = do
    (i, j) <- arbitrarySpan
    x <- arbitraryN
    return (x, i, j)

-- | Arbitrary unary rule.
arbitraryUnary :: Gen CFG.Unary
arbitraryUnary = CFG.Unary
    <$> arbitraryN
    <*> arbitraryNT

-- | Arbitrary binary rule.
arbitraryBinary :: Gen CFG.Binary
arbitraryBinary = CFG.Binary
    <$> arbitraryN
    <*> arbitraryNT
    <*> arbitraryNT

-- | Arbitrary CFG.
arbitraryCFG :: Gen CFG.CFG
arbitraryCFG = CFG.CFG
    <$> arbitrarySet unaryMax arbitraryUnary
    <*> arbitrarySet binaryMax arbitraryBinary
    <*> pure (S.fromList $ map N [1..nMax])
    <*> pure (S.fromList $ map T [1..tMax])
    <*> arbitrarySet (fromIntegral nMax) (N <$> choose (1, nMax))

-- | Arbitrary sentence of terminal symbols.
arbitrarySent :: Gen (V.Vector T)
arbitrarySent = V.fromList <$> vectorOf posMax arbitraryT

-- | Arbitrary set of active ranges.
arbitraryActive :: Gen (S.Set (Pos, Pos))
arbitraryActive = arbitrarySet activeMax arbitrarySpan

arbitraryNerfD :: Gen NerfD
arbitraryNerfD = NerfD
    <$> arbitraryCFG
    <*> arbitrarySent
    <*> arbitraryActive
    <*> arbitraryMap descMax arbitraryNode arbitraryReal
    <*> arbitraryMap descMax arbitraryUnary arbitraryReal
    <*> arbitraryMap descMax arbitraryBinary arbitraryReal

instance Arbitrary NerfD where
    arbitrary = arbitraryNerfD

(~==) :: LogReal -> LogReal -> Bool
x ~== y = 
    x == y || (1 <= z + eps && z <= 1 + eps)
  where
    z = x / y
    eps = 1.0e-10

data NodeNT = NodeNT (Either N T) Pos Pos deriving Show

instance Arbitrary NodeNT where
    arbitrary = do
        (i, j) <- arbitrarySpan
        x <- arbitraryNT
        return $ NodeNT x i j

propAlpha :: NerfD -> NodeNT -> Bool
propAlpha nd (NodeNT x i j) = (~==)
    (runNerf nd $ AF.alpha x i j)
    (runNerf nd $ AC.alpha x i j)

main :: IO ()
main = do
    -- sample (arbitrary :: Gen NodeNT)
    quickCheck propAlpha
