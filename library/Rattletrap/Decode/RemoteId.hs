module Rattletrap.Decode.RemoteId
  ( decodeRemoteIdBits
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Decode.Common
import Rattletrap.Decode.Word64le
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Word8le
import Rattletrap.Utility.Bytes

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word

decodeRemoteIdBits
  :: Word8le -> Reader.ReaderT (Int, Int, Int) DecodeBits RemoteId
decodeRemoteIdBits systemId = case word8leValue systemId of
  0 -> RemoteIdSplitscreen <$> Trans.lift (getWord32be 24)
  1 -> RemoteIdSteam <$> Trans.lift decodeWord64leBits
  2 -> RemoteIdPlayStation <$> Trans.lift decodePsName <*> decodePsBytes
  4 -> RemoteIdXbox <$> Trans.lift decodeWord64leBits
  _ -> fail ("unknown system id " <> show systemId)

decodePsName :: DecodeBits Text.Text
decodePsName = fmap
  ( Text.dropWhileEnd (== '\x00')
  . Text.decodeLatin1
  . LazyBytes.toStrict
  . reverseBytes
  )
  (getLazyByteStringBits 16)

decodePsBytes :: Reader.ReaderT (Int, Int, Int) DecodeBits [Word.Word8]
decodePsBytes = do
  (_, _, patch) <- Reader.ask
  LazyBytes.unpack
    <$> Trans.lift (getLazyByteStringBits (if patch >= 1 then 24 else 16))
