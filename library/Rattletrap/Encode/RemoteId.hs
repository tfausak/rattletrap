module Rattletrap.Encode.RemoteId
  ( putRemoteId
  ) where

import Rattletrap.Encode.Word64le
import Rattletrap.Type.RemoteId
import Rattletrap.Utility.Bytes

import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.ByteString.Lazy as ByteString

putRemoteId :: RemoteId -> BinaryBit.BitPut ()
putRemoteId remoteId = case remoteId of
  RemoteIdPlayStation name bytes -> do
    let
      rawName = ByteString.toStrict
        (reverseBytes (padBytes (16 :: Int) (encodeLatin1 name)))
    BinaryBit.putByteString rawName
    BinaryBit.putByteString (ByteString.toStrict (ByteString.pack bytes))
  RemoteIdSplitscreen word24 -> BinaryBit.putWord32be 24 word24
  RemoteIdSteam word64 -> putWord64Bits word64
  RemoteIdXbox word64 -> putWord64Bits word64
