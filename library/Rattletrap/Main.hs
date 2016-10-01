module Rattletrap.Main where

import qualified System.Environment as Environment

main :: IO ()
main = do
  args <- Environment.getArgs
  mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs _args = pure ()
