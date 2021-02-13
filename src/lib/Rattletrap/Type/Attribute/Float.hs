{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Float where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Float32le as Float32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype FloatAttribute = FloatAttribute
  { floatAttributeValue :: Float32le.Float32le
  } deriving (Eq, Show)

$(deriveJson ''FloatAttribute)

putFloatAttribute :: FloatAttribute -> BitPut ()
putFloatAttribute floatAttribute =
  Float32le.bitPut (floatAttributeValue floatAttribute)

decodeFloatAttributeBits :: BitGet FloatAttribute
decodeFloatAttributeBits = FloatAttribute <$> Float32le.bitGet
