module Rattletrap.Console.Flag where

data Flag
  = Compact
  | Fast
  | Help
  | Input FilePath
  | Mode String
  | Output FilePath
  | SkipCrc
  | Version
  deriving (Eq, Show)
