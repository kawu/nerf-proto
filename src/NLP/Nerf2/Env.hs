{-# LANGUAGE RecordWildCards #-}

-- | Environment record data types.

module NLP.Nerf2.Env
(
-- * Main
  MainEnv (..)
, labelNum
, labelVect
, labels
-- * Parameters
, ParaEnv (..)
, mkParaEnv
, phiNode
, phiBinary
, phiUnary
-- * Sentence
, SentEnv (..)
, mkSentEnv
, isActive
, inputHas
-- * Classes
, InMain (..)
, InPara (..)
, InSent (..)
-- * Layers
, Layer0 (..)
, Layer1 (..)
, Layer2 (..)
-- * CFG
, perTopU
, perDown
, perTopB
, perLeft
, perRight
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import NLP.Nerf2.Types
import qualified NLP.Nerf2.CFG as C

-- | A main Nerf environment.
data MainEnv = MainEnv
    { cfg :: C.CFG }
    deriving (Show)

-- | A vector of nonterminals.
labelVect :: InMain e => e -> U.Vector N
labelVect = U.fromList . S.toList . C.nsyms . cfg . mainEnv

-- | A number of nonterminals.
labelNum :: InMain e => e -> Int
labelNum = U.length . labelVect

-- | A list of nonterminals.
labels :: InMain e => e -> [N]
labels = U.toList . labelVect

-- | An environment related to parameters (i.e., when parameter values
-- change, the `ParaEnv` environment also changes).
data ParaEnv = ParaEnv
    { phiNodeM      :: M.Map (N, Pos, Pos) LogReal
    , phiUnaryM     :: M.Map C.Unary LogReal
    , phiBinaryM    :: M.Map C.Binary LogReal
    -- | A set of (top, down, unary potential) tuples.
    , unaryN        :: V.Vector (N, N, LogReal)
    -- | A set of (top, unary potential) tuples for a given down terminal.
    , unaryT        :: T -> V.Vector (N, LogReal)
    -- | A set of (left, top, right, binary rule potential) tuples.
    , binaryNN      :: V.Vector (N, N, N, LogReal)
    -- | A set of (left, top, potential) tuples for a given terminal.
    , binaryNT      :: T -> V.Vector (N, N, LogReal)
    -- | A set of (top, right, potential) tuples for a given terminal.
    , binaryTN      :: T -> V.Vector (N, N, LogReal)
    -- | A set of (top, potential) tuples for a given left and right terminals.
    , binaryTT      :: T -> T -> V.Vector (N, LogReal) }

instance Show ParaEnv where
    show ParaEnv{..}
        =  "phiNodeM = " ++ show phiNodeM ++ "\n"
        ++ "phiUnaryM = " ++ show phiUnaryM ++ "\n"
        ++ "phiBinaryM = " ++ show phiBinaryM

mkParaEnv
    :: M.Map (N, Pos, Pos) LogReal
    -> M.Map C.Unary LogReal
    -> M.Map C.Binary LogReal
    -> ParaEnv
mkParaEnv pn pu pb = ParaEnv
    { phiNodeM      = pn
    , phiUnaryM     = pu
    , phiBinaryM    = pb
    , unaryN        = unaN
    , unaryT        = unaT
    , binaryNN      = binNN
    , binaryNT      = binNT
    , binaryTN      = binTN
    , binaryTT      = binTT }
  where
    unaN  = V.fromList
        [ (top, down, phi)
        | ( C.Unary top (Left down)
          , phi ) <- M.toList pu ]
    unaT  = onKey . fmap V.fromList $ M.fromListWith (++)
        [ (down, [(top, phi)])
        | ( C.Unary top (Right down)
          , phi ) <- M.toList pu ]
    binNN = V.fromList
        [ (left, top, right, phi)
        | ( C.Binary top (Left left) (Left right)
          , phi ) <- M.toList pb ]
    binNT = onKey . fmap V.fromList $ M.fromListWith (++)
        [ (right, [(left, top, phi)])
        | ( C.Binary top (Left left) (Right right)
          , phi ) <- M.toList pb ]
    binTN = onKey . fmap V.fromList $ M.fromListWith (++)
        [ (left, [(top, right, phi)])
        | ( C.Binary top (Right left) (Left right)
          , phi ) <- M.toList pb ]
    binTT = onKey2 . fmap V.fromList $ M.fromListWith (++)
        [ ((left, right), [(top, phi)])
        | ( C.Binary top (Right left) (Right right)
          , phi ) <- M.toList pb ]
    onKey m k = case M.lookup k m of
        Just v  -> v
        Nothing -> V.empty
    onKey2 m k0 k1 = case M.lookup (k0, k1) m of
        Just v  -> v
        Nothing -> V.empty

-- | Potential of a tree node within the context.
phiNode :: InPara e => e -> N -> Pos -> Pos -> LogReal
phiNode env x i j =
    M.findWithDefault 1 (x, i, j) (phiNodeM $ paraEnv env)

-- | Potential of a binary rule.
phiBinary :: InPara e => e -> C.Binary -> LogReal
phiBinary env r = M.findWithDefault 1 r (phiBinaryM $ paraEnv env)

-- | Potential of an unary rule.
phiUnary :: InPara e => e -> C.Unary -> LogReal
phiUnary env u = M.findWithDefault 1 u (phiUnaryM $ paraEnv env)

-- | An environment related to input sentence.
data SentEnv = SentEnv
    { input         :: Sent
    , activeSet     :: Active
    , phiNodeMap    :: M.Map Span RVect }
    deriving (Show)

mkSentEnv :: InPara e => e -> Sent -> Active -> SentEnv
mkSentEnv env sent act = SentEnv
    { input         = sent
    , activeSet     = act
    , phiNodeMap    = nodeMap }
  where
    nodeMap =
        let xs = S.toList act
        in  M.fromList $ zip xs (map phiSpan xs)
    phiSpan (i, j) = U.fromList
        [ phiNodeEnv x i j
        | x <- labels env ]
    phiNodeEnv = phiNode env

-- | Is a given (i, j) span active?
isActive :: InSent e => e -> Pos -> Pos -> Bool
isActive env i j = S.member (i, j) (activeSet $ sentEnv env)

-- | Does the input sentence have the particular terminal
-- on the particular position?
inputHas :: InSent e => e -> Pos -> T -> Bool
inputHas env i x = case input (sentEnv env) V.!? i of
    Just (y, _) -> x == y
    Nothing     -> False

-- | A class representing environments which contain the `MainEnv` environment.
class InMain a where
    mainEnv :: a -> MainEnv

-- | A class representing environments which contain the `ParaEnv` environment.
class InMain a => InPara a where
    paraEnv :: a -> ParaEnv

-- | A class representing environments which contain the `SentEnv` environment.
class InPara a => InSent a where
    sentEnv :: a -> SentEnv

-- | First environment layer.
data Layer0 = Layer0
    { mainEnv0 :: MainEnv }
    deriving (Show)

instance InMain Layer0 where
    mainEnv = mainEnv0

-- | Second environment layer.
data Layer1 = Layer1
    { mainEnv1 :: MainEnv
    , paraEnv1 :: ParaEnv }
    deriving (Show)

instance InMain Layer1 where
    mainEnv = mainEnv1

instance InPara Layer1 where
    paraEnv = paraEnv1

-- | Third environment layer.
data Layer2 = Layer2
    { mainEnv2 :: MainEnv
    , paraEnv2 :: ParaEnv
    , sentEnv2 :: SentEnv }
    deriving (Show)

instance InMain Layer2 where
    mainEnv = mainEnv2

instance InPara Layer2 where
    paraEnv = paraEnv2

instance InSent Layer2 where
    sentEnv = sentEnv2

----------------------------------------------------------------------
-- Versions of CFG functions working on the typeclassed environment.
----------------------------------------------------------------------

-- | A set of unary rules with the given top symbol.
perTopU :: InMain e => e -> N -> [C.Unary]
perTopU = C.perTopU . cfg . mainEnv

-- | A set of unary rules with the given down symbol.
perDown :: InMain e => e -> Either N T -> [C.Unary]
perDown = C.perDown . cfg . mainEnv

-- | A set of binary rules with the given top symbol.
perTopB :: InMain e => e -> N -> [C.Binary]
perTopB = C.perTopB . cfg . mainEnv

-- | A set of binary rules with the given left symbol.
perLeft :: InMain e => e -> Either N T -> [C.Binary]
perLeft = C.perLeft . cfg . mainEnv

-- | A set of binary rules with the given right symbol.
perRight :: InMain e => e -> Either N T -> [C.Binary]
perRight = C.perRight . cfg . mainEnv
