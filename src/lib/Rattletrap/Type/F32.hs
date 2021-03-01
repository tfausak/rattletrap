module Rattletrap.Type.F32 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype F32
  = F32 Float
  deriving (Eq, Show)

instance Json.FromJSON F32 where
  parseJSON = fmap fromFloat . Json.parseJSON

instance Json.ToJSON F32 where
  toJSON = Json.toJSON . toFloat

schema :: Schema.Schema
schema = Schema.named "f32" $ Json.object [Json.pair "type" "number"]

fromFloat :: Float -> F32
fromFloat = F32

toFloat :: F32 -> Float
toFloat (F32 x) = x

bytePut :: F32 -> BytePut.BytePut
bytePut = BytePut.float . toFloat

bitPut :: F32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet F32
byteGet = fromFloat <$> ByteGet.float

bitGet :: BitGet.BitGet F32
bitGet = BitGet.fromByteGet byteGet 4
