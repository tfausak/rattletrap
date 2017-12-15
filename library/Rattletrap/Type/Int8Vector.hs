{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Int8Vector
  ( Int8Vector(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int8

data Int8Vector = Int8Vector
  { int8VectorX :: Maybe Int8
  , int8VectorY :: Maybe Int8
  , int8VectorZ :: Maybe Int8
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Int8Vector where
  parseJSON = defaultParseJson "Int8Vector"

instance ToJSON Int8Vector where
  toEncoding = defaultToEncoding "Int8Vector"
  toJSON = defaultToJson "Int8Vector"
