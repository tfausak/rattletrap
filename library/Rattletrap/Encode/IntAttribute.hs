module Rattletrap.Encode.IntAttribute
  ( putIntAttribute
  ) where

import Rattletrap.Type.IntAttribute
import Rattletrap.Encode.Int32le

import qualified Data.Binary.Bits.Put as BinaryBit

putIntAttribute :: IntAttribute -> BinaryBit.BitPut ()
putIntAttribute intAttribute = putInt32Bits (intAttributeValue intAttribute)
