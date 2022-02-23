module Rattletrap.Type.Property.Byte where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data Byte = Byte
  { key :: Str.Str
  , value :: Maybe Str.Str
  }
  deriving (Eq, Show)

instance Argo.HasCodec Byte where
  codec =
    Argo.withIdentifier "ByteProperty"
      . Argo.fromArrayCodec Argo.Forbid
      $ Byte
      <$> Argo.project key (Argo.element Argo.codec)
      <*> Argo.project value (Argo.element Argo.codec)

bytePut :: Byte -> BytePut.BytePut
bytePut byte = Str.bytePut (key byte) <> foldMap Str.bytePut (value byte)

byteGet :: ByteGet.ByteGet Byte
byteGet = ByteGet.label "Byte" $ do
  key <- ByteGet.label "key" Str.byteGet
  value <- ByteGet.label "value" $ Monad.whenMaybe
    (Str.toString key /= "OnlinePlatform_Steam")
    Str.byteGet
  pure Byte { key, value }
