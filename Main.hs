module Main where
import Regex.Genex
import System.IO
import System.Environment
import Data.Char (isDigit)

defaultRegex :: String
defaultRegex = "a(b|c)d{2,3}e*"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        [] -> do
            prog <- getProgName
            if prog == "<interactive>" then run defaultRegex else do
            