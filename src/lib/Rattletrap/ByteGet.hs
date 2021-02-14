module Rattletrap.ByteGet where

import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Word as Word

type ByteGet = Binary.Get

run :: ByteGet a -> ByteString.ByteString -> Either String a
run f x =
  case Binary.runGetOrFail f $ LazyByteString.fromStrict x of
    Left (_, _, y) -> Left y
    Right (_, _, y) -> Right y

byteString :: Int -> ByteGet ByteString.ByteString
byteString = Binary.getByteString

float :: ByteGet Float
float = Binary.getFloatle

int8 :: ByteGet Int.Int8
int8 = Binary.getInt8

int32 :: ByteGet Int.Int32
int32 = Binary.getInt32le

int64 :: ByteGet Int.Int64
int64 = Binary.getInt64le

remaining :: ByteGet LazyByteString.ByteString
remaining = Binary.getRemainingLazyByteString

word8 :: ByteGet Word.Word8
word8 = Binary.getWord8

word32 :: ByteGet Word.Word32
word32 = Binary.getWord32le

word64 :: ByteGet Word.Word64
word64 = Binary.getWord64le
