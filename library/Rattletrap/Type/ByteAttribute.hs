{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.ByteAttribute
  ( ByteAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8

newtype ByteAttribute = ByteAttribute
  { byteAttributeValue :: Word8
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON ByteAttribute where
  parseJSON = defaultParseJson "ByteAttribute"

instance ToJSON ByteAttribute where
  toEncoding = defaultToEncoding "ByteAttribute"
  toJSON = defaultToJson "ByteAttribute"
