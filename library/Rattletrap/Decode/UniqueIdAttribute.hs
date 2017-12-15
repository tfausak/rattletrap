module Rattletrap.Decode.UniqueIdAttribute
  ( getUniqueIdAttribute
  ) where

import Rattletrap.Decode.RemoteId
import Rattletrap.Decode.Word8le
import Rattletrap.Type.UniqueIdAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getUniqueIdAttribute :: (Int, Int, Int) -> BinaryBit.BitGet UniqueIdAttribute
getUniqueIdAttribute version = do
  systemId <- getWord8Bits
  remoteId <- getRemoteId version systemId
  localId <- getWord8Bits
  pure (UniqueIdAttribute systemId remoteId localId)
