{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.EnumAttribute
  ( EnumAttribute(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

newtype EnumAttribute = EnumAttribute
  { enumAttributeValue :: Word.Word16
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON EnumAttribute where
  parseJSON = defaultParseJson "EnumAttribute"

instance ToJSON EnumAttribute where
  toEncoding = defaultToEncoding "EnumAttribute"
  toJSON = defaultToJson "EnumAttribute"
