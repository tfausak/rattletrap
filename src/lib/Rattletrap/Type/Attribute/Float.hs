{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Float where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype FloatAttribute = FloatAttribute
  { floatAttributeValue :: Float32le
  } deriving (Eq, Show)

$(deriveJson ''FloatAttribute)

putFloatAttribute :: FloatAttribute -> BinaryBits.BitPut ()
putFloatAttribute floatAttribute =
  putFloat32Bits (floatAttributeValue floatAttribute)

decodeFloatAttributeBits :: BitGet FloatAttribute
decodeFloatAttributeBits = FloatAttribute <$> decodeFloat32leBits
