{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Regex.Genex.Pure (genexPure) where
import qualified Data.Text as T
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.List (intersect, (\\))
import Control.Monad
import Control.Monad.Stream
import qualified Control.Monad.Stream as Stream
import Regex.Genex.Normalize (normalize)
import Debug.Trace
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Control.Monad.State
import Control.Applicative

parse :: String -> Pattern
parse r = case parseRegex r of
    Right (pattern, _) -> pattern
    Left x -> error $ show x

genexPure :: [String] -> [String]
genexPure = map T.unpack . foldl1 intersect . map (Stream.runStream . run . normalize IntSet.empty . parse)

maxRepeat :: Int
maxRepeat = 10

each = foldl1 (<|>) . map return

run :: Pattern -> Stream T.Text
run p = case p of
    PEmpty -> pure T.empty
    PChar{..} -> isChar getPatternChar
    PAny {getPatternSet = PatternSet (Just cset) _ _ _} -> each $ map T.singleton $ Set.toList cset
    PAnyNot {getPatternSet = PatternSet (Just cset) _ _ _} -> chars $ notChars $ concatMap expandEscape $ Set.toList cset
    PQuest p -> pure T.empty