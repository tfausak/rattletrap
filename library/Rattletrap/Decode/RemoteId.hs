module Rattletrap.Decode.RemoteId
  ( getRemoteId
  ) where

import Rattletrap.Type.RemoteId
import Rattletrap.Decode.Word64
import Rattletrap.Type.Word8le
import Rattletrap.Utility.Bytes

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

getRemoteId :: (Int, Int, Int) -> Word8le -> BinaryBit.BitGet RemoteId
getRemoteId (_, _, patchVersion) systemId = case word8leValue systemId of
  0 -> do
    word24 <- BinaryBit.getWord32be 24
    pure (RemoteIdSplitscreen word24)
  1 -> do
    word64 <- getWord64Bits
    pure (RemoteIdSteam word64)
  2 -> do
    rawName <- BinaryBit.getLazyByteString 16
    let
      name = Text.dropWhileEnd
        (== '\x00')
        (Encoding.decodeLatin1 (ByteString.toStrict (reverseBytes rawName)))
      numBytes = if patchVersion >= 1 then 24 else 16
    bytes <- BinaryBit.getLazyByteString numBytes
    pure (RemoteIdPlayStation name (ByteString.unpack bytes))
  4 -> do
    word64 <- getWord64Bits
    pure (RemoteIdXbox word64)
  _ -> fail ("unknown system id " ++ show systemId)
