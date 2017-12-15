{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.ClassMapping
  ( ClassMapping(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32
import Rattletrap.Type.Text

data ClassMapping = ClassMapping
  { classMappingName :: Text
  , classMappingStreamId :: Word32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON ClassMapping where
  parseJSON = defaultParseJson "ClassMapping"

instance ToJSON ClassMapping where
  toEncoding = defaultToEncoding "ClassMapping"
  toJSON = defaultToJson "ClassMapping"
