module Rattletrap.Decode.RemoteId
  ( decodeRemoteIdBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word64le
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Word64le
import Rattletrap.Type.Word8le
import Rattletrap.Utility.Bytes

import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word

decodeRemoteIdBits :: (Int, Int, Int) -> Word8le -> DecodeBits RemoteId
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

decodePsName :: DecodeBits Text.Text
decodePsName = fmap
  (Text.dropWhileEnd (== '\x00') . Text.decodeLatin1 . reverseBytes)
  (getByteStringBits 16)

decodePsBytes :: (Int, Int, Int) -> DecodeBits [Word.Word8]
decodePsBytes version = Bytes.unpack
  <$> getByteStringBits (if version >= (868, 20, 1) then 24 else 16)

getWord256 :: DecodeBits (Word64le, Word64le, Word64le, Word64le)
getWord256 = do
  a <- decodeWord64leBits
  b <- decodeWord64leBits
  c <- decodeWord64leBits
  d <- decodeWord64leBits
  pure (a, b, c, d)
