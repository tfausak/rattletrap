{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.RemoteId where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word64le
import Rattletrap.Encode.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import Rattletrap.Type.Word8le

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
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
  deriving (Eq, Show)

$(deriveJson ''RemoteId)

putRemoteId :: RemoteId -> BitPut ()
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
  :: Word64le -> Word64le -> Word64le -> Word64le -> BitPut ()
putWord256 a b c d = do
  putWord64Bits a
  putWord64Bits b
  putWord64Bits c
  putWord64Bits d

decodeRemoteIdBits :: (Int, Int, Int) -> Word8le -> BitGet RemoteId
decodeRemoteIdBits version systemId = case word8leValue systemId of
  0 -> RemoteIdSplitscreen <$> getBitsLE 24
  1 -> RemoteIdSteam <$> decodeWord64leBits
  2 -> RemoteIdPlayStation <$> decodePsName <*> decodePsBytes version
  4 -> RemoteIdXbox <$> decodeWord64leBits
  6 -> do
    (a, b, c, d) <- getWord256
    pure $ RemoteIdSwitch a b c d
  7 -> if version >= (868, 24, 10)
    then RemoteIdPsyNet . Left <$> decodeWord64leBits
    else RemoteIdPsyNet . Right <$> getWord256
  11 -> RemoteIdEpic <$> decodeStrBits
  _ -> fail ("[RT09] unknown system id " <> show systemId)

decodePsName :: BitGet Text.Text
decodePsName = fmap
  (Text.dropWhileEnd (== '\x00') . Text.decodeLatin1 . reverseBytes)
  (getByteStringBits 16)

decodePsBytes :: (Int, Int, Int) -> BitGet [Word.Word8]
decodePsBytes version = Bytes.unpack
  <$> getByteStringBits (if version >= (868, 20, 1) then 24 else 16)

getWord256 :: BitGet (Word64le, Word64le, Word64le, Word64le)
getWord256 = do
  a <- decodeWord64leBits
  b <- decodeWord64leBits
  c <- decodeWord64leBits
  d <- decodeWord64leBits
  pure (a, b, c, d)
