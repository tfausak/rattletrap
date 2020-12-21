module Rattletrap.Decode.UniqueIdAttribute
  ( decodeUniqueIdAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.RemoteId
import Rattletrap.Decode.Word8le
import Rattletrap.Type.UniqueIdAttribute

decodeUniqueIdAttributeBits :: (Int, Int, Int) -> DecodeBits UniqueIdAttribute
decodeUniqueIdAttributeBits version = do
  systemId <- decodeWord8leBits
  UniqueIdAttribute systemId
    <$> decodeRemoteIdBits version systemId
    <*> decodeWord8leBits
