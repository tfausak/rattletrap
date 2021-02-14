{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.UniqueId where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data UniqueIdAttribute = UniqueIdAttribute
  { systemId :: Word8le.Word8le
  , remoteId :: RemoteId.RemoteId
  , localId :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''UniqueIdAttribute)

bitPut :: UniqueIdAttribute -> BitPut ()
bitPut uniqueIdAttribute = do
  Word8le.bitPut $ systemId uniqueIdAttribute
  RemoteId.bitPut (remoteId uniqueIdAttribute)
  Word8le.bitPut $ localId uniqueIdAttribute

bitGet :: (Int, Int, Int) -> BitGet UniqueIdAttribute
bitGet version = do
  systemId_ <- Word8le.bitGet
  UniqueIdAttribute systemId_
    <$> RemoteId.bitGet version systemId_
    <*> Word8le.bitGet
