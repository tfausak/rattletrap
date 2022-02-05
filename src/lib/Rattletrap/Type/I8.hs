module Rattletrap.Type.I8 where

import qualified Data.Int as Int
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Vendor.Argo as Argo

newtype I8
  = I8 Int.Int8
  deriving (Eq, Show)

instance Argo.HasCodec I8 where
  codec = Argo.identified $ Argo.map fromInt8 toInt8 Argo.codec

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
