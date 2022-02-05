module Rattletrap.Type.Property.Float where

import Prelude hiding (Float)
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Vendor.Argo as Argo

newtype FloatP -- TODO
  = Float F32.F32
  deriving (Eq, Show)

instance Argo.HasCodec FloatP where
  codec = Argo.identified $ Argo.map fromF32 toF32 Argo.codec

fromF32 :: F32.F32 -> FloatP
fromF32 = Float

toF32 :: FloatP -> F32.F32
toF32 (Float x) = x

bytePut :: FloatP -> BytePut.BytePut
bytePut = F32.bytePut . toF32

byteGet :: ByteGet.ByteGet FloatP
byteGet = ByteGet.label "Float" $ fmap fromF32 F32.byteGet
