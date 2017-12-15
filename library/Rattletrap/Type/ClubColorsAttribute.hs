{-# LANGUAGE TemplateHaskell #-}

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
  } deriving (Eq, Ord, Show)

$(deriveJson ''ClubColorsAttribute)
