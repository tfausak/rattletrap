module Rattletrap.Type.U32 where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype U32
  = U32 Word.Word32
  deriving (Eq, Ord, Show)

instance Json.FromJSON U32 where
  parseJSON = fmap fromWord32 . Json.parseJSON

instance Json.ToJSON U32 where
  toJSON = Json.toJSON . toWord32

schema :: Schema.Schema
schema = Schema.named "u32" $ Json.object
  [ Json.pair "type" "integer"
  , Json.pair "minimum" (minBound :: Word.Word32)
  , Json.pair "maximum" (maxBound :: Word.Word32)
  ]

fromWord32 :: Word.Word32 -> U32
fromWord32 = U32

toWord32 :: U32 -> Word.Word32
toWord32 (U32 x) = x

bytePut :: U32 -> BytePut.BytePut
bytePut = BytePut.word32 . toWord32

bitPut :: U32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet U32
byteGet = fromWord32 <$> ByteGet.word32

bitGet :: BitGet.BitGet U32
bitGet = BitGet.fromByteGet byteGet 4
