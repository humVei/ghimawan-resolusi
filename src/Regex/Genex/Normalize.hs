
{-# LANGUAGE ImplicitParams, NamedFieldPuns, PatternGuards #-}
module Regex.Genex.Normalize (normalize) where
import Data.Set (toList, Set)
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

type BackReferences = IntSet

-- | Normalize a regex into @strong star normal form@, as defined in the paper
--   @Simplifying Regular Expressions: A Quantitative Perspective@.
normalize :: BackReferences -> Pattern -> Pattern
normalize refs p = black $ let ?refs = refs in simplify p

nullable :: Pattern -> Bool
nullable pat = case pat of
    PGroup _ p -> nullable p
    PQuest{} -> True
    POr ps -> any nullable ps
    PConcat ps -> all nullable ps
    PBound 0 _ _ -> True
    PBound _ _ _ -> False
    PStar{} -> True
    PEmpty -> True
    _ -> False

white :: Pattern -> Pattern
white pat = case pat of
    PQuest p -> white p
    PStar _ p -> white p
    PGroup x p -> PGroup x $ white p
    POr ps -> POr (map white ps)
    PConcat ps -> if nullable pat
        then POr (map white ps)
        else pat
    PPlus p -> if nullable pat
        then PConcat [p, white p]
        else pat
    _ -> pat

black :: Pattern -> Pattern
black pat = case pat of
    POr ps -> POr (map black ps)
    PConcat ps -> PConcat (map black ps)
    PGroup x p -> PGroup x $ black p