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
import Control.Monad.