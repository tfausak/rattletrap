module Rattletrap.Type.Property where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Vendor.Argo as Argo

data Property = Property
  { kind :: Str.Str
  , size :: U64.U64
  -- ^ Not used.
  , value :: PropertyValue.PropertyValue Property
  }
  deriving (Eq, Show)

instance Argo.HasCodec Property where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Property
      <$> Argo.required kind "kind"
      <*> Argo.required size "size"
      <*> Argo.required value "value"

bytePut :: Property -> BytePut.BytePut
bytePut x =
  Str.bytePut (kind x) <> U64.bytePut (size x) <> PropertyValue.bytePut
    bytePut
    (value x)

byteGet :: ByteGet.ByteGet Property
byteGet = ByteGet.label "Property" $ do
  kind <- ByteGet.label "kind" Str.byteGet
  size <- ByteGet.label "size" U64.byteGet
  value <- ByteGet.label "value" $ PropertyValue.byteGet byteGet kind
  pure Property { kind, size, value }
