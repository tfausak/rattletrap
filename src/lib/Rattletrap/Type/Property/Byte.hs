module Rattletrap.Type.Property.Byte where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data ByteP = Byte -- TODO
  { key :: Str.Str
  , value :: Maybe Str.Str
  }
  deriving (Eq, Show)

instance Argo.HasCodec ByteP where
  codec =
    Argo.identified
      . Argo.fromArrayCodec Argo.Forbid
      $ Byte
      <$> Argo.project key (Argo.element Argo.codec)
      <*> Argo.project value (Argo.element Argo.codec)

bytePut :: ByteP -> BytePut.BytePut
bytePut byte = Str.bytePut (key byte) <> foldMap Str.bytePut (value byte)

byteGet :: ByteGet.ByteGet ByteP
byteGet = ByteGet.label "Byte" $ do
  key <- ByteGet.label "key" Str.byteGet
  value <- ByteGet.label "value" $ Monad.whenMaybe
    (Str.toString key /= "OnlinePlatform_Steam")
    Str.byteGet
  pure Byte { key, value }
