module Rattletrap.Type.Float32
  ( Float32(..)
  ) where

newtype Float32 = Float32
  { float32Value :: Float
  } deriving (Eq, Ord, Show)
