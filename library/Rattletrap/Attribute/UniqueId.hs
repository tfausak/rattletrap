module Rattletrap.Attribute.UniqueId where

import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8
import Rattletrap.Encode.Word8
import Rattletrap.RemoteId

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data UniqueIdAttribute = UniqueIdAttribute
  { uniqueIdAttributeSystemId :: Word8
  , uniqueIdAttributeRemoteId :: RemoteId
  , uniqueIdAttributeLocalId :: Word8
  } deriving (Eq, Ord, Show)

getUniqueIdAttribute :: (Int, Int, Int) -> BinaryBit.BitGet UniqueIdAttribute
getUniqueIdAttribute version = do
  systemId <- getWord8Bits
  remoteId <- getRemoteId version systemId
  localId <- getWord8Bits
  pure (UniqueIdAttribute systemId remoteId localId)

putUniqueIdAttribute :: UniqueIdAttribute -> BinaryBit.BitPut ()
putUniqueIdAttribute uniqueIdAttribute = do
  putWord8Bits (uniqueIdAttributeSystemId uniqueIdAttribute)
  putRemoteId (uniqueIdAttributeRemoteId uniqueIdAttribute)
  putWord8Bits (uniqueIdAttributeLocalId uniqueIdAttribute)
