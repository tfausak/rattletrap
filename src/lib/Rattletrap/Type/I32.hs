module Rattletrap.Type.I32 where

import qualified Data.Int as Int
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype I32
  = I32 Int.Int32
  deriving (Eq, Show)

instance Json.FromJSON I32 where
  parseJSON = fmap fromInt32 . Json.parseJSON

instance Json.ToJSON I32 where
  toJSON = Json.toJSON . toInt32

schema :: Schema.Schema
schema =
  Schema.named "i32" $
    Json.object
      [ Json.pair "type" "integer",
        Json.pair "minimum" (minBound :: Int.Int32),
        Json.pair "maximum" (maxBound :: Int.Int32)
      ]

fromInt32 :: Int.Int32 -> I32
fromInt32 = I32

toInt32 :: I32 -> Int.Int32
toInt32 (I32 x) = x

bytePut :: I32 -> BytePut.BytePut
bytePut = BytePut.int32 . toInt32

bitPut :: I32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet I32
byteGet = ByteGet.label "I32" $ fmap fromInt32 ByteGet.int32

bitGet :: BitGet.BitGet I32
bitGet = BitGet.fromByteGet byteGet 4
