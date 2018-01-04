module Rattletrap.Decode.RemoteId
  ( decodeRemoteIdBits
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Decode.Bitstream
import Rattletrap.Decode.Common
import Rattletrap.Decode.Word64le
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Word8le
import Rattletrap.Utility.Bytes

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word

decodeRemoteIdBits :: (Int, Int, Int) -> Word8le -> DecodeBits RemoteId
decodeRemoteIdBits (_, _, patch) systemId = case word8leValue systemId of
  0 -> RemoteIdSplitscreen <$> getWord32be 24
  1 -> RemoteIdSteam <$> decodeWord64leBits
  2 -> RemoteIdPlayStation <$> decodePsName <*> decodePsBytes patch
  4 -> RemoteIdXbox <$> decodeWord64leBits
  6 -> RemoteIdSwitch <$> decodeBitstreamBits 256
  _ -> fail ("unknown system id " <> show systemId)

decodePsName :: DecodeBits Text.Text
decodePsName = fmap
  ( Text.dropWhileEnd (== '\x00')
  . Text.decodeLatin1
  . LazyBytes.toStrict
  . reverseBytes
  )
  (getLazyByteStringBits 16)

decodePsBytes :: Int -> DecodeBits [Word.Word8]
decodePsBytes patch =
  LazyBytes.unpack <$> getLazyByteStringBits (if patch >= 1 then 24 else 16)
