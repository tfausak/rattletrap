{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClubColorsAttribute
  ( ClubColorsAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le

data ClubColorsAttribute = ClubColorsAttribute
  { clubColorsAttributeBlueFlag :: Bool
  , clubColorsAttributeBlueColor :: Word8le
  , clubColorsAttributeOrangeFlag :: Bool
  , clubColorsAttributeOrangeColor :: Word8le
  } deriving (Eq, Ord, Show)

$(deriveJson ''ClubColorsAttribute)
