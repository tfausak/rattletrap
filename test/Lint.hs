module Main
  ( main
  ) where

import qualified Language.Haskell.HLint3 as HLint
import qualified System.Exit as Exit

main :: IO ()
main = do
  ideas <- HLint.hlint ["lint", "--cross", "."]
  if null ideas
    then Exit.exitSuccess
    else Exit.exitFailure
