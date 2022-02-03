module Rattletrap.Type.U64 where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Vendor.Argo as Argo
import qualified Text.Read as Read

newtype U64
  = U64 Word.Word64
  deriving (Eq, Show)

instance Argo.HasCodec U64 where
  codec = Argo.identified $ Argo.mapMaybe
    (fmap fromWord64 . Read.readMaybe)
    (Just . show . toWord64)
    Argo.codec

fromWord64 :: Word.Word64 -> U64
fromWord64 = U64

toWord64 :: U64 -> Word.Word64
toWord64 (U64 x) = x

bytePut :: U64 -> BytePut.BytePut
bytePut = BytePut.word64 . toWord64

bitPut :: U64 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet U64
byteGet = ByteGet.label "U64" $ fmap fromWord64 ByteGet.word64

bitGet :: BitGet.BitGet U64
bitGet = BitGet.fromByteGet byteGet 8
