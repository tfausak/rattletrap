module Rattletrap.Type.U32 where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Vendor.Argo as Argo

newtype U32
  = U32 Word.Word32
  deriving (Eq, Ord, Show)

instance Argo.HasCodec U32 where
  codec = Argo.map fromWord32 toWord32 Argo.codec

fromWord32 :: Word.Word32 -> U32
fromWord32 = U32

toWord32 :: U32 -> Word.Word32
toWord32 (U32 x) = x

bytePut :: U32 -> BytePut.BytePut
bytePut = BytePut.word32 . toWord32

bitPut :: U32 -> BitPut.BitPut
bitPut = BitPut.fromBytePut . bytePut

byteGet :: ByteGet.ByteGet U32
byteGet = ByteGet.label "U32" $ fmap fromWord32 ByteGet.word32

bitGet :: BitGet.BitGet U32
bitGet = BitGet.fromByteGet byteGet 4
