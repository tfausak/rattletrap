{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.RemoteId where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word64le
import Rattletrap.Encode.Common
import Rattletrap.Utility.Bytes

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.ByteString as Bytes

data RemoteId
  = RemoteIdPlayStation Text [Word8]
  | RemoteIdPsyNet (Either Word64le (Word64le, Word64le, Word64le, Word64le))
  | RemoteIdSplitscreen Word32
  -- ^ Really only 24 bits.
  | RemoteIdSteam Word64le
  | RemoteIdSwitch Word64le Word64le Word64le Word64le
  | RemoteIdXbox Word64le
  | RemoteIdEpic Str
  deriving (Eq, Ord, Show)

$(deriveJson ''RemoteId)

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
