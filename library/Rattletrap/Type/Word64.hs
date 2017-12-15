{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Word64
  ( Word64(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

newtype Word64 = Word64
  { word64Value :: Word.Word64
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Word64 where
  parseJSON = defaultParseJson "Word64"

instance ToJSON Word64 where
  toEncoding = defaultToEncoding "Word64"
  toJSON = defaultToJson "Word64"
