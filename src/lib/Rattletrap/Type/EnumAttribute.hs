{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.EnumAttribute where

import Rattletrap.Type.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype EnumAttribute = EnumAttribute
  { enumAttributeValue :: Word16
  } deriving (Eq, Ord, Show)

$(deriveJson ''EnumAttribute)

putEnumAttribute :: EnumAttribute -> BinaryBits.BitPut ()
putEnumAttribute enumAttribute =
  putBitsLE 11 (enumAttributeValue enumAttribute)
