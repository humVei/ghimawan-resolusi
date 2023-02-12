
{-# LANGUAGE ImplicitParams, NamedFieldPuns, ParallelListComp, PatternGuards #-}
{-|

This module and the accompanying 'genex' program finds all permutations
of strings that matches every input regular expressions, ordered from
shortest to longest, with full support for back references ('\1' .. '\9')
and word boundaries ('\b').

It requires the @z3@ or @yices@ binary in PATH. The latter may be downloaded from:
<http://yices.csl.sri.com/download-yices2.shtml>

-}
module Regex.Genex (Model(..), genex, genexPure, genexPrint, genexModels, genexWith, regexMatch) where
import Data.SBV
import Data.SBV.Internals (SBV)
import Data.Set (toList)
import Data.Monoid
import Control.Monad.State
import qualified Data.Char
import qualified Regex.Genex.Pure as Pure
import Text.Regex.TDFA.Pattern
import Regex.Genex.Normalize (normalize)
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Given a list of regular repressions, returns all possible strings that matches every one of them.
-- Guarantees to return shorter strings before longer ones.
genex :: [String] -> IO [String]
genex = let ?maxRepeat = maxRepeatDefault
        in genexWith getString

-- | A match consists of a string (list of codepoints), and a rank representing alternation order.
data Model = Model
    { modelChars :: [Word8]
    , modelRank :: Word64
    }
    deriving (Show, Eq, Ord)

-- | Same as 'genex', but with the entire model returned instead.
genexModels :: [String] -> IO [Model]
genexModels = let ?maxRepeat = maxRepeatDefault
              in genexWith (getStringWith id)

-- | Same as 'genexModels', but print the models to standard output instead.
genexPrint :: [String] -> IO ()
genexPrint = let ?maxRepeat = maxRepeatDefault
             in genexWith displayString

-- | A pure and much faster variant of 'genex', but without support for
--   back-references, anchors or word boundaries.
-- Does not guarantee orders about length of strings.
-- Does not depend on the external @yices@ SMT solver.
genexPure :: [String] -> [String]
genexPure = Pure.genexPure

type Len = Word16
type SChar = SWord8
type Str = [SChar]
type Offset = SBV Len
type Flips = [SWord64]
type Captures = SFunArray Word8 Len
type Hits = Word16

maxHits :: Hits
maxHits = maxBound -- 65535

-- controlled by an implicit parameter, but this is the default
-- when instantiated from functions that do not expose the implicit
-- parameter to the user
maxRepeatDefault :: Int
maxRepeatDefault = 3 -- 7 and 15 are also good

maxLength :: Len
maxLength = maxBound -- 65535

-- lengths p = let ?grp = mempty in IntSet.toList . fst $ runState (possibleLengths $ parse p) mempty

minLen :: (?maxRepeat :: Int, ?grp :: GroupLens) => Pattern -> Int
minLen p = case p of
    PEscape {getPatternChar = ch}
        | Data.Char.isDigit ch -> let num = charToDigit ch in
            IntSet.findMin (IntMap.findWithDefault (IntSet.singleton 0) num ?grp)
    _ -> IntSet.findMin . fst $ runState (possibleLengths p) mempty

parse :: String -> Pattern
parse r = case parseRegex r of
    Right (pattern, _) -> pattern
    Left x -> error $ show x

type GroupLens = IntMap IntSet
type BackReferences = IntSet

possibleLengths :: (?maxRepeat :: Int, ?grp :: GroupLens) => Pattern -> State (GroupLens, BackReferences) IntSet
possibleLengths pat = case pat of
    _ | isOne pat -> one
    PGroup (Just idx) p -> do
        lenP <- possibleLengths p
        modify $ \(g, b) -> (IntMap.insert idx lenP g, b)
        return lenP
    PGroup _ p -> possibleLengths p
    PCarat{} -> zero
    PDollar{} -> zero
    PQuest p -> maybeGroup p (`mappend` zeroSet)
    POr ps -> fmap mconcat $ mapM possibleLengths ps
    PConcat [] -> zero
    PConcat ps -> fmap (foldl1 sumSets) (mapM possibleLengths ps)
    PEscape {getPatternChar = ch}
        | ch `elem` "ntrfaedwsWSD" -> one
        | ch `elem` "b" -> zero
        | Data.Char.isDigit ch -> do
            let num = charToDigit ch
            modify $ \(g, b) -> (g, IntSet.insert num b)