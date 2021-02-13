{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Enum where

import Rattletrap.Type.Common
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

newtype EnumAttribute = EnumAttribute
  { enumAttributeValue :: Word16
  } deriving (Eq, Show)

$(deriveJson ''EnumAttribute)

putEnumAttribute :: EnumAttribute -> BitPut ()
putEnumAttribute enumAttribute =
  putBitsLE 11 (enumAttributeValue enumAttribute)

decodeEnumAttributeBits :: BitGet EnumAttribute
decodeEnumAttributeBits = EnumAttribute <$> getBitsLE 11
