{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int64 where

import Rattletrap.Type.Common
import Rattletrap.Type.Int64le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype Int64Attribute = Int64Attribute
  { int64AttributeValue :: Int64le
  } deriving (Eq, Show)

$(deriveJson ''Int64Attribute)

putInt64Attribute :: Int64Attribute -> BitPut ()
putInt64Attribute int64Attribute =
  putInt64Bits (int64AttributeValue int64Attribute)

decodeInt64AttributeBits :: BitGet Int64Attribute
decodeInt64AttributeBits = Int64Attribute <$> decodeInt64leBits
