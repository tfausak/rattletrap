{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.UniqueId where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data UniqueIdAttribute = UniqueIdAttribute
  { uniqueIdAttributeSystemId :: Word8le.Word8le
  , uniqueIdAttributeRemoteId :: RemoteId.RemoteId
  , uniqueIdAttributeLocalId :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''UniqueIdAttribute)

putUniqueIdAttribute :: UniqueIdAttribute -> BitPut ()
putUniqueIdAttribute uniqueIdAttribute = do
  Word8le.bitPut $ uniqueIdAttributeSystemId uniqueIdAttribute
  RemoteId.bitPut (uniqueIdAttributeRemoteId uniqueIdAttribute)
  Word8le.bitPut $ uniqueIdAttributeLocalId uniqueIdAttribute

decodeUniqueIdAttributeBits :: (Int, Int, Int) -> BitGet UniqueIdAttribute
decodeUniqueIdAttributeBits version = do
  systemId <- Word8le.bitGet
  UniqueIdAttribute systemId
    <$> RemoteId.bitGet version systemId
    <*> Word8le.bitGet
