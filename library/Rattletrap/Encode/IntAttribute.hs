module Rattletrap.Encode.IntAttribute
  ( putIntAttribute
  )
where

import Rattletrap.Encode.Int32le
import Rattletrap.Type.IntAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putIntAttribute :: IntAttribute -> BinaryBits.BitPut ()
putIntAttribute intAttribute = putInt32Bits (intAttributeValue intAttribute)
