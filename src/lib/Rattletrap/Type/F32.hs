module Rattletrap.Type.F32 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Vendor.Argo as Argo

newtype F32
  = F32 Float
  deriving (Eq, Show)

instance Argo.HasCodec F32 where
  codec = Argo.map fromFloat toFloat Argo.codec

fromFloat :: Float -> F32
fromFloat = F32

toFloat :: F32 -> Float
toFloat (F32 x) = x

bytePut :: F32 -> BytePut.BytePut
bytePut = BytePut.float . toFloat

bitPut :: F32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet F32
byteGet = ByteGet.label "F32" $ fmap fromFloat ByteGet.float

bitGet :: BitGet.BitGet F32
bitGet = BitGet.fromByteGet byteGet 4
