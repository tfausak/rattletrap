module Rattletrap.Attribute.QWord where

import Rattletrap.Type.Word64
import Rattletrap.Decode.Word64
import Rattletrap.Encode.Word64

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype QWordAttribute = QWordAttribute
  { qWordAttributeValue :: Word64
  } deriving (Eq, Ord, Show)

getQWordAttribute :: BinaryBit.BitGet QWordAttribute
getQWordAttribute = do
  value <- getWord64Bits
  pure (QWordAttribute value)

putQWordAttribute :: QWordAttribute -> BinaryBit.BitPut ()
putQWordAttribute qWordAttribute =
  putWord64Bits (qWordAttributeValue qWordAttribute)
