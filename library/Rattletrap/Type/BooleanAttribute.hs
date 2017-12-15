{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.BooleanAttribute
  ( BooleanAttribute(..)
  ) where

import Rattletrap.Type.Common

newtype BooleanAttribute = BooleanAttribute
  { booleanAttributeValue :: Bool
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON BooleanAttribute where
  parseJSON = defaultParseJson "BooleanAttribute"

instance ToJSON BooleanAttribute where
  toEncoding = defaultToEncoding "BooleanAttribute"
  toJSON = defaultToJson "BooleanAttribute"
