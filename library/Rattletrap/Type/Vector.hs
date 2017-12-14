module Rattletrap.Type.Vector
  ( Vector(..)
  ) where

data Vector = Vector
  { vectorBias :: Word
  , vectorX :: Int
  , vectorY :: Int
  , vectorZ :: Int
  } deriving (Eq, Ord, Show)
