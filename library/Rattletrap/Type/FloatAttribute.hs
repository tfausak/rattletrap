{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.FloatAttribute
  ( FloatAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32

newtype FloatAttribute = FloatAttribute
  { floatAttributeValue :: Float32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON FloatAttribute where
  parseJSON = defaultParseJson "FloatAttribute"

instance ToJSON FloatAttribute where
  toEncoding = defaultToEncoding "FloatAttribute"
  toJSON = defaultToJson "FloatAttribute"
