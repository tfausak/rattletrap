module Rattletrap.Encode.Int64Attribute
  ( putInt64Attribute
  )
where

import Rattletrap.Encode.Int64le
import Rattletrap.Type.Int64Attribute

import qualified Data.Binary.Bits.Put as BinaryBits

putInt64Attribute :: Int64Attribute -> BinaryBits.BitPut ()
putInt64Attribute int64Attribute =
  putInt64Bits (int64AttributeValue int64Attribute)
