module Rattletrap.Type.Property.Float where

import Prelude hiding (Float)
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Vendor.Argo as Argo

newtype Float
  = Float F32.F32
  deriving (Eq, Show)

instance Argo.HasCodec Float where
  codec = Argo.identified $ Argo.map fromF32 toF32 Argo.codec

fromF32 :: F32.F32 -> Float
fromF32 = Float

toF32 :: Float -> F32.F32
toF32 (Float x) = x

bytePut :: Float -> BytePut.BytePut
bytePut = F32.bytePut . toF32

byteGet :: ByteGet.ByteGet Float
byteGet = ByteGet.label "Float" $ fmap fromF32 F32.byteGet
