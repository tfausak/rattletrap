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

instance Json.FromJSON Float where
  parseJSON = fmap fromF32 . Json.parseJSON

instance Json.ToJSON Float where
  toJSON = Json.toJSON . toF32

schema :: Schema.Schema
schema = F32.schema

bytePut :: Float -> BytePut.BytePut
bytePut = F32.bytePut . toF32

byteGet :: ByteGet.ByteGet Float
byteGet = fmap fromF32 F32.byteGet
