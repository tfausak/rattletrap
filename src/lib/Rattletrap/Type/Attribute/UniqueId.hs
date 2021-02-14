{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.UniqueId where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data UniqueIdAttribute = UniqueIdAttribute
  { systemId :: U8.U8
  , remoteId :: RemoteId.RemoteId
  , localId :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''UniqueIdAttribute)

bitPut :: UniqueIdAttribute -> BitPut ()
bitPut uniqueIdAttribute = do
  U8.bitPut $ systemId uniqueIdAttribute
  RemoteId.bitPut (remoteId uniqueIdAttribute)
  U8.bitPut $ localId uniqueIdAttribute

bitGet :: (Int, Int, Int) -> BitGet UniqueIdAttribute
bitGet version = do
  systemId_ <- U8.bitGet
  UniqueIdAttribute systemId_
    <$> RemoteId.bitGet version systemId_
    <*> U8.bitGet
