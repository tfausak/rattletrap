module Rattletrap.BytePut where

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Word as Word

type BytePut = Binary.Put

toByteString :: BytePut -> ByteString.ByteString
toByteString = LazyByteString.toStrict . toLazyByteString

toLazyByteString :: BytePut -> LazyByteString.ByteString
toLazyByteString = Builder.toLazyByteString . toBuilder

toBuilder :: BytePut -> Builder.Builder
toBuilder = Binary.execPut

byteString :: ByteString.ByteString -> BytePut
byteString = Binary.putByteString

float :: Float -> BytePut
float = Binary.putFloatle

int8 :: Int.Int8 -> BytePut
int8 = Binary.putInt8

int32 :: Int.Int32 -> BytePut
int32 = Binary.putInt32le

int64 :: Int.Int64 -> BytePut
int64 = Binary.putInt64le

word8 :: Word.Word8 -> BytePut
word8 = Binary.putWord8

word32 :: Word.Word32 -> BytePut
word32 = Binary.putWord32le

word64 :: Word.Word64 -> BytePut
word64 = Binary.putWord64le
