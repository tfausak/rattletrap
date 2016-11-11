module Rattletrap.RemoteId where

import Rattletrap.Word64
import Rattletrap.Primitive.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Word as Word

data RemoteId
  = PlayStationId [Word.Word8]
  | SplitscreenId [Word.Word8]
  | SteamId Word64
  | XboxId Word64
  deriving (Eq, Ord, Show)

getRemoteId :: Word8 -> BinaryBit.BitGet RemoteId
getRemoteId systemId =
  case word8Value systemId of
    0 -> do
      bytes <- BinaryBit.getLazyByteString 3
      pure (SplitscreenId (ByteString.unpack bytes))
    1 -> do
      word64 <- getWord64Bits
      pure (SteamId word64)
    2 -> do
      bytes <- BinaryBit.getLazyByteString 32
      pure (PlayStationId (ByteString.unpack bytes))
    4 -> do
      word64 <- getWord64Bits
      pure (XboxId word64)
    _ -> fail ("unknown system id " ++ show systemId)

putRemoteId :: RemoteId -> BinaryBit.BitPut ()
putRemoteId remoteId =
  case remoteId of
    PlayStationId bytes ->
      BinaryBit.putByteString (ByteString.toStrict (ByteString.pack bytes))
    SplitscreenId bytes ->
      BinaryBit.putByteString (ByteString.toStrict (ByteString.pack bytes))
    SteamId word64 -> putWord64Bits word64
    XboxId word64 -> putWord64Bits word64
