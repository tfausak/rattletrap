module Rattletrap.Console.Flag where

data Flag
  = Compact
  | Fast
  | Help
  | Input FilePath
  | Mode String
  | Output FilePath
  | Version
  deriving (Eq, Show)
