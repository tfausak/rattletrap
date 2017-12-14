module Rattletrap.Encode.RemoteId
  ( putRemoteId
  ) where

import Rattletrap.Type.RemoteId
import Rattletrap.Encode.Word64
import Rattletrap.Utility

import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.ByteString.Lazy as ByteString

putRemoteId :: RemoteId -> BinaryBit.BitPut ()
putRemoteId remoteId = case remoteId of
  PlayStationId name bytes -> do
    let
      rawName = ByteString.toStrict
        (reverseBytes (padBytes (16 :: Int) (encodeLatin1 name)))
    BinaryBit.putByteString rawName
    BinaryBit.putByteString (ByteString.toStrict (ByteString.pack bytes))
  SplitscreenId word24 -> BinaryBit.putWord32be 24 word24
  SteamId word64 -> putWord64Bits word64
  XboxId word64 -> putWord64Bits word64
