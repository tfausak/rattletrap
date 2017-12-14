{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.IntAttribute
  ( IntAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32

newtype IntAttribute = IntAttribute
  { intAttributeValue :: Int32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON IntAttribute where
  parseJSON = defaultParseJson "IntAttribute"

instance ToJSON IntAttribute where
  toEncoding = defaultToEncoding "IntAttribute"
  toJSON = defaultToJson "IntAttribute"
