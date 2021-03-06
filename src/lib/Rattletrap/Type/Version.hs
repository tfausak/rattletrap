module Rattletrap.Type.Version where

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  }
  deriving (Eq, Show)

atLeast :: Int -> Int -> Int -> Version -> Bool
atLeast m n p v = major v >= m && minor v >= n && patch v >= p
