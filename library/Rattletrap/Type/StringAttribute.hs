{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.StringAttribute
  ( StringAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Text

newtype StringAttribute = StringAttribute
  { stringAttributeValue :: Text
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON StringAttribute where
  parseJSON = defaultParseJson "StringAttribute"

instance ToJSON StringAttribute where
  toEncoding = defaultToEncoding "StringAttribute"
  toJSON = defaultToJson "StringAttribute"
