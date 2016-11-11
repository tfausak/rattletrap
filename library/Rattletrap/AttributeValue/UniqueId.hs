module Rattletrap.AttributeValue.UniqueId where

import Rattletrap.RemoteId
import Rattletrap.Primitive.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data UniqueIdAttributeValue = UniqueIdAttributeValue
  { uniqueIdAttributeValueSystemId :: Word8
  , uniqueIdAttributeValueRemoteId :: RemoteId
  , uniqueIdAttributeValueLocalId :: Word8
  } deriving (Eq, Ord, Show)

getUniqueIdAttributeValue :: BinaryBit.BitGet UniqueIdAttributeValue
getUniqueIdAttributeValue = do
  systemId <- getWord8Bits
  remoteId <- getRemoteId systemId
  localId <- getWord8Bits
  pure (UniqueIdAttributeValue systemId remoteId localId)

putUniqueIdAttributeValue :: UniqueIdAttributeValue -> BinaryBit.BitPut ()
putUniqueIdAttributeValue uniqueIdAttributeValue = do
  putWord8Bits (uniqueIdAttributeValueSystemId uniqueIdAttributeValue)
  putRemoteId (uniqueIdAttributeValueRemoteId uniqueIdAttributeValue)
  putWord8Bits (uniqueIdAttributeValueLocalId uniqueIdAttributeValue)
