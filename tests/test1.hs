{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

import Control.Applicative ((<$>), (<*>), pure)
import System.Exit (exitFailure)
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import NLP.Nerf2.Types
import NLP.Nerf2.Monad
import qualified NLP.Nerf2.LogReal as L
import qualified NLP.Nerf2.CFG as CFG
import qualified NLP.Nerf2.Env as Env

import qualified NLP.Nerf2.Alpha as A
import qualified NLP.Nerf2.Alpha.Ref as AF
import qualified NLP.Nerf2.Alpha.Rec as AC

import qualified NLP.Nerf2.Forest.Set as F
import qualified NLP.Nerf2.Forest.Phi as F

import qualified NLP.Nerf2.Gamma as G
import qualified NLP.Nerf2.Gamma.Ref as GF

import qualified NLP.Nerf2.Delta as D
import qualified NLP.Nerf2.Delta.Ref as DF
import qualified NLP.Nerf2.Forest.Set as F

import Debug.Trace (trace)

-- | QuickCheck parameters.
posMax      = 5
tMax        = 4
nMax        = 4
activeMax   = 25
unaryMax    = 10
binaryMax   = 10
nodeMax     = 10
edgeMax     = 10
phiMax      = 10.0 :: Double

-- | Arbitrary set of maximum k elements.
arbitrarySet :: Ord a => Int -> Gen a -> Gen (S.Set a)
arbitrarySet k g = S.fromList <$> vectorOf k g

-- | Arbitrary map of maximum k elements.
arbitraryMap :: Ord a => Int -> Gen a -> Gen b -> Gen (M.Map a b)
arbitraryMap k g g' = (M.fromList.) . zip
    <$> vectorOf k g
    <*> vectorOf k g'

-- | Arbitrary map from a set.
mapFromSet :: Ord a => S.Set a -> Gen b -> Gen (M.Map a b)
mapFromSet s g = do
    let xs = S.toList s
    ys <- vectorOf (S.size s) g
    return $ M.fromList (zip xs ys)

arbitraryPos :: Gen Pos
arbitraryPos = choose (0, posMax-1)

arbitrarySpan :: Gen (Pos, Pos)
arbitrarySpan =
    let pair = (,) <$> arbitraryPos <*> arbitraryPos
    in  pair `suchThat` \(i, j) -> i <= j

arbitraryReal :: Gen LogReal
arbitraryReal = L.logToLogReal <$> choose (-phiMax, phiMax)

-- | Arbitrary terminal.
arbitraryT :: Gen T 
arbitraryT = T <$> choose (0, tMax-1)

-- | Arbitrary nonterminal.
arbitraryN :: Gen N 
arbitraryN = N <$> choose (0, nMax-1)

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

-- | Arbitrary edge given a set of starting symbols.
arbitraryEdge :: S.Set N -> Gen (N, N)
arbitraryEdge s = (,)
    <$> arbitraryN `suchThat` (`S.member` s)
    <*> arbitraryN `suchThat` (`S.member` s)

-- | Arbitrary CFG.
arbitraryCFG :: Gen CFG.CFG
arbitraryCFG = CFG.CFG
    <$> arbitrarySet unaryMax arbitraryUnary
    <*> arbitrarySet binaryMax arbitraryBinary
    <*> pure (S.fromList $ map N [0..nMax-1])
    <*> pure (S.fromList $ map T [0..tMax-1])
    <*> arbitrarySet (fromIntegral nMax) (N <$> choose (0, nMax-1))
    -- <*> pure (S.fromList $ map N [0..nMax-1])

-- | Arbitrary sentence.
arbitrarySent :: Gen Sent
arbitrarySent =
    let seg = (,) <$> arbitraryT <*> pure U.empty
    in  V.fromList <$> vectorOf posMax seg

-- | Arbitrary set of active ranges.
arbitraryActive :: Gen (S.Set (Pos, Pos))
arbitraryActive = arbitrarySet activeMax arbitrarySpan

-- | Arbitrary parameter environment.
arbitraryParaEnv :: CFG.CFG -> Gen Env.ParaEnv
arbitraryParaEnv cfg = do
    nodeM   <- arbitraryMap nodeMax arbitraryNode arbitraryReal
    unaryM  <- mapFromSet (CFG.unary cfg) arbitraryReal
    binaryM <- mapFromSet (CFG.binary cfg) arbitraryReal
    edgeM   <- arbitraryMap edgeMax
         (arbitraryEdge $ CFG.start cfg) arbitraryReal
    return $ Env.mkParaEnv nodeM unaryM binaryM edgeM

-- | Arbitrary sentence environment.
arbitrarySentEnv :: Env.Layer1 -> Gen Env.SentEnv
arbitrarySentEnv paraEnv = Env.mkSentEnv paraEnv
    <$> arbitrarySent
    <*> arbitraryActive

-- | Arbitrary layer2.
arbitraryLayer2 :: Gen Env.Layer2
arbitraryLayer2 = do
    cfg     <- arbitraryCFG
    let mainEnv = Env.MainEnv cfg
    paraEnv <- arbitraryParaEnv cfg
    sentEnv <- arbitrarySentEnv (Env.Layer1 mainEnv paraEnv)
    return $ Env.Layer2 mainEnv paraEnv sentEnv

instance Arbitrary Env.Layer2 where
    arbitrary = arbitraryLayer2

(~==) :: LogReal -> LogReal -> Bool
x ~== y = 
    x == y || (1 <= z + eps && z <= 1 + eps)
  where
    z = x / y
    eps = 1.0e-10

data NodeN = NodeN N Pos Pos deriving Show

instance Arbitrary NodeN where
    arbitrary = do
        x <- arbitraryN
        (i, j) <- arbitrarySpan
        return $ NodeN x i j

data Point = Point N Pos deriving Show

instance Arbitrary Point where
    arbitrary = do
        x <- arbitraryN
        i <- arbitraryPos
        return $ Point x i

propAlpha :: Env.Layer2 -> NodeN -> Bool
propAlpha env (NodeN x i j) =
    trace (show (r0, r1, r2)) $ r0 ~== r1 && r1 ~== r2
  where
    r0 = runNerf env $ A.alphaAtM x i j
    r1 = AF.alpha env (Left x) i j
    r2 = AC.alpha env (Left x) i j

propGamma :: Env.Layer2 -> Point -> Bool
propGamma env (Point x i) =
    trace (show (r0, r1)) $ r0 ~== r1
  where
    r0 = runNerf env $ G.bsAtM x i
    r1 = GF.gamma env x i

propDelta :: Env.Layer2 -> Point -> Bool
propDelta env (Point x i) =
    trace (show (r0, r1)) $ r0 ~== r1
  where
    r0 = runNerf env $ D.bsAtM x i
    r1 = DF.delta env x i

-- | A set of forests spanning the (0, i) range should include
-- all forests spanned over the (0, i-1) range.
propSubForest :: Env.Layer2 -> Point -> Bool
propSubForest e (Point x i)
    | i < 1     = True
    | otherwise = S.fromList (F.forestSetF e x $ i - 1)
        `S.isSubsetOf` S.fromList (F.forestSetF e x i)

-- | A set of forests spanning the (i, n-1) range should include
-- all forests spanned over the (i+1, n-1) range.
propSubForest' :: Env.Layer2 -> Point -> Bool
propSubForest' e (Point x i)
    | i > Env.inputLength e - 2 = True
    | otherwise = S.fromList (F.forestSetB e x $ i + 1)
        `S.isSubsetOf` S.fromList (F.forestSetB e x i)

-- | A set of forests must include the empty forest.
propEmptyForest :: Env.Layer2 -> Bool
propEmptyForest e = [] `elem` F.forestSet e

-- | A set of forests is the same no matter which method
-- is used, forward or backward computation.
propForestSet :: Env.Layer2 -> Bool
propForestSet e =
    trace (show f1 ++ "    #    " ++ show f2) $ f1 == f2
  where
    f1 = F.forestSet e
    f2 = F.forestSet' e

-- | Normalization factor should be the same no matter which
-- method is used, forward or backward computation.
propNorm :: Env.Layer2 -> Bool
propNorm env = do
    trace (show (z0, z1, z2)) $ z0 ~== z1 && z1 ~== z2
  where
    z0 = F.norm env
    z1 = runNerf env $ G.normTest
    z2 = runNerf env $ D.normTest

-- | Check property and `exitFailure` on failure.
check :: Testable prop => prop -> IO ()
check prop = quickCheckResult prop >>= \x -> case x of
    Success _ _ _   -> return ()
    _               -> exitFailure

main :: IO ()
main = do
--     check propAlpha
--     check propGamma
--     check propDelta
--     check propEmptyForest
--     check propSubForest
--     check propSubForest'
--     check propForestSet
    check propNorm
