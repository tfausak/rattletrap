module Rattletrap.Type.Version where

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  } deriving (Eq, Show)
