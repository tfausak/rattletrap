{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Boolean where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype BooleanAttribute = BooleanAttribute
  { booleanAttributeValue :: Bool
  } deriving (Eq, Show)

$(deriveJson ''BooleanAttribute)

putBooleanAttribute :: BooleanAttribute -> BitPut ()
putBooleanAttribute booleanAttribute =
  BinaryBits.putBool (booleanAttributeValue booleanAttribute)

decodeBooleanAttributeBits :: BitGet BooleanAttribute
decodeBooleanAttributeBits = BooleanAttribute <$> getBool
