module Rattletrap.Decode.Common
  ( Decode
  , DecodeBits
  , decodeWhen
  , getLazyByteStringBits
  , getWord8Bits
  , runDecode
  , runDecodeBits
  , toBits
  , Binary.getFloatle
  , Binary.getLazyByteString
  , Binary.getInt8
  , Binary.getInt32le
  , Binary.getInt64le
  , Binary.getWord8
  , Binary.getWord32le
  , Binary.getWord64le
  , BinaryBits.getBool
  , BinaryBits.getWord16be
  , BinaryBits.getWord32be
  )
where

import qualified Control.Applicative as Applicative
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Word as Word
import qualified Rattletrap.Utility.Bytes as Utility

type Decode = Binary.Get

type DecodeBits = BinaryBits.BitGet

decodeWhen
  :: (Applicative m, Applicative.Alternative f) => Bool -> m a -> m (f a)
decodeWhen p f = if p then fmap pure f else pure Applicative.empty

getLazyByteStringBits :: Int -> DecodeBits LazyBytes.ByteString
getLazyByteStringBits = BinaryBits.getLazyByteString

getWord8Bits :: Int -> DecodeBits Word.Word8
getWord8Bits = BinaryBits.getWord8

runDecode :: Decode a -> LazyBytes.ByteString -> Either String a
runDecode decode bytes = case Binary.runGetOrFail decode bytes of
  Left (_, _, x) -> fail x
  Right (_, _, x) -> pure x

runDecodeBits :: DecodeBits a -> LazyBytes.ByteString -> Either String a
runDecodeBits = runDecode . BinaryBits.runBitGet

toBits :: Decode a -> Int -> DecodeBits a
toBits decode =
  fmap (Binary.runGet decode . Utility.reverseBytes)
    . BinaryBits.getLazyByteString
