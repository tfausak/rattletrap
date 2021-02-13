{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Float where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype FloatAttribute = FloatAttribute
  { floatAttributeValue :: Float32le
  } deriving (Eq, Show)

$(deriveJson ''FloatAttribute)

putFloatAttribute :: FloatAttribute -> BitPut ()
putFloatAttribute floatAttribute =
  putFloat32Bits (floatAttributeValue floatAttribute)

decodeFloatAttributeBits :: BitGet FloatAttribute
decodeFloatAttributeBits = FloatAttribute <$> decodeFloat32leBits
