{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.QWordAttribute
  ( QWordAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word64

newtype QWordAttribute = QWordAttribute
  { qWordAttributeValue :: Word64
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON QWordAttribute where
  parseJSON = defaultParseJson "QWordAttribute"

instance ToJSON QWordAttribute where
  toEncoding = defaultToEncoding "QWordAttribute"
  toJSON = defaultToJson "QWordAttribute"
