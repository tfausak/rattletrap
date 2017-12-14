{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Vector
  ( Vector(..)
  ) where

import Rattletrap.Type.Common

data Vector = Vector
  { vectorBias :: Word
  , vectorX :: Int
  , vectorY :: Int
  , vectorZ :: Int
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Vector where
  parseJSON = defaultParseJson "Vector"

instance ToJSON Vector where
  toEncoding = defaultToEncoding "Vector"
  toJSON = defaultToJson "Vector"
