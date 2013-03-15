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

import Debug.Trace (trace)

-- | QuickCheck parameters.
posMax      = 5
tMax        = 4
nMax        = 4
activeMax   = 25
unaryMax    = 10
binaryMax   = 10
nodeMax     = 10
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

-- | Arbitrary CFG.
arbitraryCFG :: Gen CFG.CFG
arbitraryCFG = CFG.CFG
    <$> arbitrarySet unaryMax arbitraryUnary
    <*> arbitrarySet binaryMax arbitraryBinary
    <*> pure (S.fromList $ map N [0..nMax-1])
    <*> pure (S.fromList $ map T [0..tMax-1])
    <*> arbitrarySet (fromIntegral nMax) (N <$> choose (0, nMax-1))

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
    return $ Env.mkParaEnv nodeM unaryM binaryM

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
        (i, j) <- arbitrarySpan
        x <- arbitraryN
        return $ NodeN x i j

propAlpha :: Env.Layer2 -> NodeN -> Bool
propAlpha env (NodeN x i j) =
    trace (show (r0, r1, r2)) $ r0 ~== r1 && r1 ~== r2
  where
    r0 = runNerf env $ A.alphaAt x i j
    r1 = AF.alpha env (Left x) i j
    r2 = AC.alpha env (Left x) i j

main :: IO ()
main = do
    -- sample (arbitrary :: Gen NodeN)
    res <- quickCheckResult propAlpha
    case res of
        Success _ _ _   -> return ()
        _               -> exitFailure
