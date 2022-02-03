module Rattletrap.Type.U8 where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Vendor.Argo as Argo

newtype U8
  = U8 Word.Word8
  deriving (Eq, Show)

instance Argo.HasCodec U8 where
  codec = Argo.identified $ Argo.map fromWord8 toWord8 Argo.codec

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
