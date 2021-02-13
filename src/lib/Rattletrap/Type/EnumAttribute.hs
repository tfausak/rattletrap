{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.EnumAttribute where

import Rattletrap.Type.Common
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype EnumAttribute = EnumAttribute
  { enumAttributeValue :: Word16
  } deriving (Eq, Show)

$(deriveJson ''EnumAttribute)

putEnumAttribute :: EnumAttribute -> BinaryBits.BitPut ()
putEnumAttribute enumAttribute =
  putBitsLE 11 (enumAttributeValue enumAttribute)

decodeEnumAttributeBits :: DecodeBits EnumAttribute
decodeEnumAttributeBits = EnumAttribute <$> getBitsLE 11
