module Rattletrap.Type.I64 where

import qualified Data.Int as Int
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Text.Read as Read
import qualified Rattletrap.Vendor.Argo as Argo

newtype I64
  = I64 Int.Int64
  deriving (Eq, Show)

instance Argo.HasCodec I64 where
  codec = Argo.mapMaybe (fmap fromInt64 . Read.readMaybe) (Just . show . toInt64) Argo.codec

fromInt64 :: Int.Int64 -> I64
fromInt64 = I64

toInt64 :: I64 -> Int.Int64
toInt64 (I64 x) = x

bytePut :: I64 -> BytePut.BytePut
bytePut = BytePut.int64 . toInt64

bitPut :: I64 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet I64
byteGet = fmap fromInt64 ByteGet.int64

bitGet :: BitGet.BitGet I64
bitGet = BitGet.fromByteGet byteGet 8
