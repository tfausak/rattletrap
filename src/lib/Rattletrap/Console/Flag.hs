module Rattletrap.Console.Flag where

data Flag
  = Compact
  | Fast
  | Help
  | Input FilePath
  | Mode String
  | Output FilePath
  | Schema
  | SkipCrc
  | Version
  deriving (Eq, Show)
