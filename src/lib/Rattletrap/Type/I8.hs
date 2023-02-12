module Rattletrap.Type.I8 where

import qualified Data.Int as Int
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype I8
  = I8 Int.Int8
  deriving (Eq, Show)

instance Json.FromJSON I8 where
  parseJSON = fmap fromInt8 . Json.parseJSON

instance Json.ToJSON I8 where
  toJSON = Json.toJSON . toInt8

schema :: Schema.Schema
schema =
  Schema.named "i8" $
    Json.object
      [ Json.pair "type" "integer",
        Json.pair "minimum" (minBound :: Int.Int8),
        Json.pair "maximum" (maxBound :: Int.Int8)
      ]

fromInt8 :: Int.Int8 -> I8
fromInt8 = I8

toInt8 :: I8 -> Int.Int8
toInt8 (I8 x) = x

bytePut :: I8 -> BytePut.BytePut
bytePut = BytePut.int8 . toInt8

bitPut :: I8 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet I8
byteGet = ByteGet.label "I8" $ fmap fromInt8 ByteGet.int8

bitGet :: BitGet.BitGet I8
bitGet = BitGet.fromByteGet byteGet 1
