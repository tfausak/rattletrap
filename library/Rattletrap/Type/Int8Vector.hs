module Rattletrap.Type.Int8Vector
  ( Int8Vector(..)
  ) where

import Rattletrap.Primitive.Int8

data Int8Vector = Int8Vector
  { int8VectorX :: Maybe Int8
  , int8VectorY :: Maybe Int8
  , int8VectorZ :: Maybe Int8
  } deriving (Eq, Ord, Show)
