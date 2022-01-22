module Rattletrap.Type.RemoteId.Splitscreen where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Vendor.Argo as Argo

newtype Splitscreen
  = Splitscreen Word.Word32
  deriving (Eq, Show)

instance Argo.HasCodec Splitscreen where
  codec = Argo.map fromWord32 toWord32 Argo.codec

fromWord32 :: Word.Word32 -> Splitscreen
fromWord32 = Splitscreen

toWord32 :: Splitscreen -> Word.Word32
toWord32 (Splitscreen x) = x

bitPut :: Splitscreen -> BitPut.BitPut
bitPut = BitPut.bits 24 . toWord32

bitGet :: BitGet.BitGet Splitscreen
bitGet = BitGet.label "Splitscreen" . fmap fromWord32 $ BitGet.bits 24
