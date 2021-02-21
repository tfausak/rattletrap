module Rattletrap.Type.I32 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import Rattletrap.Type.Common
import qualified Data.Aeson as Aeson
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype I32
  = I32 Int32
  deriving (Eq, Show)

$(deriveJson ''I32)

schema :: Schema.Schema
schema = Schema.named "i32" $ Aeson.object
  [ Json.pair "type" "integer"
  , Json.pair "minimum" (minBound :: Int32)
  , Json.pair "maximum" (maxBound :: Int32)
  ]

fromInt32 :: Int32 -> I32
fromInt32 = I32

toInt32 :: I32 -> Int32
toInt32 (I32 x) = x

bytePut :: I32 -> BytePut.BytePut
bytePut = BytePut.int32 . toInt32

bitPut :: I32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet I32
byteGet = fromInt32 <$> ByteGet.int32

bitGet :: BitGet.BitGet I32
bitGet = BitGet.fromByteGet byteGet 4
