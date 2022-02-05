module Rattletrap.Type.Attribute.Byte where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Vendor.Argo as Argo

newtype ByteA = Byte -- TODO
  { value :: U8.U8
  } deriving (Eq, Show)

instance Argo.HasCodec ByteA where
  codec = Argo.identified $ Argo.map Byte value Argo.codec

bitPut :: ByteA -> BitPut.BitPut
bitPut byteAttribute = U8.bitPut (value byteAttribute)

bitGet :: BitGet.BitGet ByteA
bitGet = BitGet.label "Byte" $ do
  value <- BitGet.label "value" U8.bitGet
  pure Byte { value }
