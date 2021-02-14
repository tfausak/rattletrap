module Rattletrap.BytePut where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Word as Word

type BytePut = Builder.Builder

toByteString :: BytePut -> ByteString.ByteString
toByteString = LazyByteString.toStrict . toLazyByteString

toLazyByteString :: BytePut -> LazyByteString.ByteString
toLazyByteString = Builder.toLazyByteString

byteString :: ByteString.ByteString -> BytePut
byteString = Builder.byteString

float :: Float -> BytePut
float = Builder.floatLE

int8 :: Int.Int8 -> BytePut
int8 = Builder.int8

int32 :: Int.Int32 -> BytePut
int32 = Builder.int32LE

int64 :: Int.Int64 -> BytePut
int64 = Builder.int64LE

word8 :: Word.Word8 -> BytePut
word8 = Builder.word8

word32 :: Word.Word32 -> BytePut
word32 = Builder.word32LE

word64 :: Word.Word64 -> BytePut
word64 = Builder.word64LE
