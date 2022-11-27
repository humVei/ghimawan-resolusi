{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Regex.Genex.Pure (genexPure) where
import qualified Data.Text as T
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.List (intersect, (\\))
import Control.Mon