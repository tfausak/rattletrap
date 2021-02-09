module Rattletrap.Encode.RemoteId
  ( putRemoteId
  ) where

import Rattletrap.Encode.Common
import Rattletrap.Encode.Str
import Rattletrap.Encode.Word64le
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Word64le
import Rattletrap.Utility.Bytes

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.ByteString as Bytes

putRemoteId :: RemoteId -> BinaryBits.BitPut ()
putRemoteId remoteId = case remoteId of
  RemoteIdPlayStation name bytes -> do
    let rawName = reverseBytes (padBytes (16 :: Int) (encodeLatin1 name))
    BinaryBits.putByteString rawName
    BinaryBits.putByteString (Bytes.pack bytes)
  RemoteIdPsyNet e -> case e of
    Left l -> putWord64Bits l
    Right (a, b, c, d) -> putWord256 a b c d
  RemoteIdSplitscreen word24 -> putBitsLE 24 word24
  RemoteIdSteam word64 -> putWord64Bits word64
  RemoteIdSwitch a b c d -> putWord256 a b c d
  RemoteIdXbox word64 -> putWord64Bits word64
  RemoteIdEpic str -> putTextBits str

putWord256
  :: Word64le -> Word64le -> Word64le -> Word64le -> BinaryBits.BitPut ()
putWord256 a b c d = do
  putWord64Bits a
  putWord64Bits b
  putWord64Bits c
  putWord64Bits d
