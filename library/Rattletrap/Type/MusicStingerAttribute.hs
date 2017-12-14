{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.MusicStingerAttribute
  ( MusicStingerAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32
import Rattletrap.Type.Word8

data MusicStingerAttribute = MusicStingerAttribute
  { musicStingerAttributeFlag :: Bool
  , musicStingerAttributeCue :: Word32
  , musicStingerAttributeTrigger :: Word8
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON MusicStingerAttribute where
  parseJSON = defaultParseJson "MusicStingerAttribute"

instance ToJSON MusicStingerAttribute where
  toEncoding = defaultToEncoding "MusicStingerAttribute"
  toJSON = defaultToJson "MusicStingerAttribute"
