module Rattletrap.Encode.QWordAttribute
  ( putQWordAttribute
  ) where

import Rattletrap.Encode.Word64le
import Rattletrap.Type.QWordAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putQWordAttribute :: QWordAttribute -> BinaryBit.BitPut ()
putQWordAttribute qWordAttribute =
  putWord64Bits (qWordAttributeValue qWordAttribute)
