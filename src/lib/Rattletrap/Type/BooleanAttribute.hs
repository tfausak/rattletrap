{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.BooleanAttribute where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype BooleanAttribute = BooleanAttribute
  { booleanAttributeValue :: Bool
  } deriving (Eq, Show)

$(deriveJson ''BooleanAttribute)

putBooleanAttribute :: BooleanAttribute -> BinaryBits.BitPut ()
putBooleanAttribute booleanAttribute =
  BinaryBits.putBool (booleanAttributeValue booleanAttribute)

decodeBooleanAttributeBits :: DecodeBits BooleanAttribute
decodeBooleanAttributeBits = BooleanAttribute <$> getBool
