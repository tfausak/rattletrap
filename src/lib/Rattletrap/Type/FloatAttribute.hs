{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.FloatAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le

import qualified Data.Binary.Bits.Put as BinaryBits

newtype FloatAttribute = FloatAttribute
  { floatAttributeValue :: Float32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''FloatAttribute)

putFloatAttribute :: FloatAttribute -> BinaryBits.BitPut ()
putFloatAttribute floatAttribute =
  putFloat32Bits (floatAttributeValue floatAttribute)
