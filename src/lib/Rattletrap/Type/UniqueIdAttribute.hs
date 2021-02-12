{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.UniqueIdAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Word8le

import qualified Data.Binary.Bits.Put as BinaryBits

data UniqueIdAttribute = UniqueIdAttribute
  { uniqueIdAttributeSystemId :: Word8le
  , uniqueIdAttributeRemoteId :: RemoteId
  , uniqueIdAttributeLocalId :: Word8le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''UniqueIdAttribute)

putUniqueIdAttribute :: UniqueIdAttribute -> BinaryBits.BitPut ()
putUniqueIdAttribute uniqueIdAttribute = do
  putWord8Bits (uniqueIdAttributeSystemId uniqueIdAttribute)
  putRemoteId (uniqueIdAttributeRemoteId uniqueIdAttribute)
  putWord8Bits (uniqueIdAttributeLocalId uniqueIdAttribute)
