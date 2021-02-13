{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int64Attribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Int64le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

newtype Int64Attribute = Int64Attribute
  { int64AttributeValue :: Int64le
  } deriving (Eq, Show)

$(deriveJson ''Int64Attribute)

putInt64Attribute :: Int64Attribute -> BinaryBits.BitPut ()
putInt64Attribute int64Attribute =
  putInt64Bits (int64AttributeValue int64Attribute)

decodeInt64AttributeBits :: DecodeBits Int64Attribute
decodeInt64AttributeBits = Int64Attribute <$> decodeInt64leBits
