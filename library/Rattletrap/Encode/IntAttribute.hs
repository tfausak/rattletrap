module Rattletrap.Encode.IntAttribute
  ( putIntAttribute
  ) where

import Rattletrap.Encode.Int32le
import Rattletrap.Type.IntAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putIntAttribute :: IntAttribute -> BinaryBit.BitPut ()
putIntAttribute intAttribute = putInt32Bits (intAttributeValue intAttribute)
