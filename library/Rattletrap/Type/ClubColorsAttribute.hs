{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.ClubColorsAttribute
  ( ClubColorsAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8

data ClubColorsAttribute = ClubColorsAttribute
  { clubColorsAttributeBlueFlag :: Bool
  , clubColorsAttributeBlueColor :: Word8
  , clubColorsAttributeOrangeFlag :: Bool
  , clubColorsAttributeOrangeColor :: Word8
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON ClubColorsAttribute where
  parseJSON = defaultParseJson "ClubColorsAttribute"

instance ToJSON ClubColorsAttribute where
  toEncoding = defaultToEncoding "ClubColorsAttribute"
  toJSON = defaultToJson "ClubColorsAttribute"
