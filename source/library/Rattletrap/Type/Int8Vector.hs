{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int8Vector
  ( Int8Vector(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Int8le

data Int8Vector = Int8Vector
  { int8VectorX :: Maybe Int8le
  , int8VectorY :: Maybe Int8le
  , int8VectorZ :: Maybe Int8le
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int8Vector)
