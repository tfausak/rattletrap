{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype IntAttribute = IntAttribute
  { intAttributeValue :: Int32le
  } deriving (Eq, Show)

$(deriveJson ''IntAttribute)

putIntAttribute :: IntAttribute -> BitPut ()
putIntAttribute intAttribute = putInt32Bits (intAttributeValue intAttribute)

decodeIntAttributeBits :: BitGet IntAttribute
decodeIntAttributeBits = IntAttribute <$> decodeInt32leBits
