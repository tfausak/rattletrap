module Rattletrap.Type.U8 where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype U8
  = U8 Word.Word8
  deriving (Eq, Show)

instance Json.FromValue U8 where
  fromValue = fmap fromWord8 . Json.fromValue

instance Json.ToValue U8 where
  toValue = Json.toValue . toWord8

schema :: Schema.Schema
schema = Schema.named "u8" $ Json.object
  [ Json.pair "type" "integer"
  , Json.pair "minimum" (minBound :: Word.Word8)
  , Json.pair "maximum" (maxBound :: Word.Word8)
  ]

fromWord8 :: Word.Word8 -> U8
fromWord8 = U8

toWord8 :: U8 -> Word.Word8
toWord8 (U8 x) = x

bytePut :: U8 -> BytePut.BytePut
bytePut = BytePut.word8 . toWord8

bitPut :: U8 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet U8
byteGet = ByteGet.label "U8" $ fmap fromWord8 ByteGet.word8

bitGet :: BitGet.BitGet U8
bitGet = BitGet.fromByteGet byteGet 1
