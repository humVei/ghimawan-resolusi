
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
            gets $ (IntMap.findWithDefault (IntMap.findWithDefault (error $ "No such capture: " ++ [ch]) num ?grp) num) . fst
        | Data.Char.isAlpha ch -> error $ "Unsupported escape: " ++ [ch]
        | otherwise -> one
    PBound low (Just high) p -> manyTimes p low high
    PBound low _ p -> manyTimes p low (low + ?maxRepeat)
    PPlus p -> manyTimes p 1 (?maxRepeat+1)
    PStar _ p -> manyTimes p 0 ?maxRepeat
    PEmpty -> zero
    _ -> error $ show pat
    where
    one = return $ IntSet.singleton 1
    zero = return $ IntSet.singleton 0
    zeroSet = IntSet.singleton 0
    sumSets s1 s2 = IntSet.unions [ IntSet.map (+elm) s2 | elm <- IntSet.elems s1 ]
    manyTimes p low high = maybeGroup p $ \lenP -> IntSet.unions
        [ foldl sumSets (IntSet.singleton 0) (replicate i lenP)
        | i <- [low..high]
        ]
    maybeGroup p@(PGroup (Just idx) _) f = do
        lenP <- possibleLengths p
        let lenP' = f lenP
        modify $ \(g, b) -> (IntMap.insert idx lenP' g, b)
        return lenP'
    maybeGroup p f = fmap f (possibleLengths p)

charToDigit :: Char -> Int
charToDigit ch = Data.Char.ord ch - Data.Char.ord '0'

exactMatch :: (?maxRepeat :: Int, ?pats :: [(Pattern, GroupLens)]) => Len -> Symbolic SBool
exactMatch len = do
    str <- mkExistVars $ fromEnum len
    initialFlips <- mkExistVars 1
    captureAt <- newArray_ (Just minBound)
    captureLen <- newArray_ (Just minBound)
    let ?str = str
    let initialStatus = Status
            { ok = true
            , pos = strLen
            , flips = initialFlips
            , captureAt = captureAt
            , captureLen = captureLen
            }
        strLen = literal len
        runPat s (pat, groupLens) = let ?pat = pat in let ?grp = groupLens in
            ite (ok s &&& pos s .== strLen)
                (match s{ pos = 0, captureAt, captureLen })
                s{ ok = false, pos = maxBound, flips = [maxBound] }
    let Status{ ok, pos, flips } = foldl runPat initialStatus ?pats
    return (bAll (.== 0) flips &&& pos .== strLen &&& ok)

data Status = Status
    { ok :: SBool
    , pos :: Offset
    , flips :: Flips
    , captureAt :: Captures
    , captureLen :: Captures
    }

instance Mergeable Status where
  symbolicMerge f t s1 s2 = Status
    { ok = symbolicMerge f t (ok s1) (ok s2)
    , pos = symbolicMerge f t (pos s1) (pos s2)
    , flips = symbolicMerge f t (flips s1) (flips s2)
    , captureAt = symbolicMerge f t (captureAt s1) (captureAt s2)
    , captureLen = symbolicMerge f t (captureLen s1) (captureLen s2)
    }

choice :: (?str :: Str, ?pat :: Pattern) => Flips -> [Flips -> Status] -> Status
choice _ [] = error "X"
choice flips [a] = a flips
choice flips [a, b] = ite (lsb flip) (b flips') (a flips')
    where
    flip = head flips
    flips' = [flip `shiftR` 1]
choice flips xs = select (map ($ flips') xs) (head xs [thisFlip]){ ok = false } thisFlip
    where
    bits = log2 $ length xs
    flips' = [head flips `shiftR` bits]
    thisFlip = head flips `shiftL` (64 - bits) `shiftR` (64 - bits)

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 ((n + 1) `div` 2)

writeCapture :: Captures -> Int -> Offset -> Captures
writeCapture cap idx val = writeArray cap (toEnum idx) val

readCapture :: Captures -> Int -> Offset
readCapture a = readArray a . toEnum
    
isOne :: Pattern -> Bool
isOne PChar{} = True
isOne PDot{} = True
isOne PAny {} = True
isOne PAnyNot {} = True
isOne (PGroup Nothing p) = isOne p
isOne PEscape {getPatternChar = ch}
    | ch `elem` "ntrfaedwsWSD" = True
    | ch `elem` "b" = False
    | Data.Char.isDigit ch = False
    | Data.Char.isAlpha ch = error $ "Unsupported escape: " ++ [ch]
    | otherwise = True
isOne _ = False

matchOne :: (?pat :: Pattern) => SChar -> SBool
matchOne cur = case ?pat of
    PChar {getPatternChar = ch} -> isChar ch
    PDot{} -> isDot
    PGroup Nothing p -> let ?pat = p in matchOne cur
    PAny {getPatternSet = pset} -> case pset of
        PatternSet (Just cset) _ _ _ -> oneOf $ toList cset
        _ -> error "TODO"
    PAnyNot {getPatternSet = pset} -> case pset of
        PatternSet (Just cset) _ _ _ -> noneOf $ toList cset
        _ -> error "TODO"
    PEscape {getPatternChar = ch} -> case ch of
        'n' -> isChar '\n'
        't' -> isChar '\t'
        'r' -> isChar '\r'
        'f' -> isChar '\f'
        'a' -> isChar '\a'
        'e' -> isChar '\ESC'
        'd' -> isDigit
        'w' -> isWordChar
        's' -> isWhiteSpace
        'W' -> (isDot &&& bnot isWordChar)
        'S' -> (isDot &&& bnot isWhiteSpace)
        'D' -> (isDot &&& bnot isDigit)
        _   -> isChar ch
    _ -> false
    where
    ord = toEnum . Data.Char.ord
    isChar ch = cur .== ord ch
    isDot = (cur .>= ord ' ' &&& cur .<= ord '~')
    oneOf cs = bOr [ ord ch .== cur | ch <- cs ]
    noneOf cs = bAnd ((cur .>= ord ' ') : (cur .<= ord '~') : [ ord ch ./= cur | ch <- cs ])
    isDigit = (ord '0' .<= cur &&& ord '9' .>= cur)
    isWordChar = (cur .>= ord 'A' &&& cur .<= ord 'Z')
             ||| (cur .>= ord 'a' &&& cur .<= ord 'z')
             ||| (cur .== ord '_')
    isWhiteSpace = cur .== 32 ||| (9 .<= cur &&& 13 .>= cur &&& 11 ./= cur)