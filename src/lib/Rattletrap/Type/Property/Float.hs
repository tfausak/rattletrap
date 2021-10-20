module Rattletrap.Type.Property.Float where

import Prelude hiding (Float)
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Utility.Json as Json

newtype Float
  = Float F32.F32
  deriving (Eq, Show)

fromF32 :: F32.F32 -> Float
fromF32 = Float

toF32 :: Float -> F32.F32
toF32 (Float x) = x

instance Json.FromValue Float where
  fromValue = fmap fromF32 . Json.fromValue

instance Json.ToValue Float where
  toValue = Json.toValue . toF32

schema :: Schema.Schema
schema = F32.schema

bytePut :: Float -> BytePut.BytePut
bytePut = F32.bytePut . toF32

byteGet :: ByteGet.ByteGet Float
byteGet = ByteGet.label "Float" $ fmap fromF32 F32.byteGet
