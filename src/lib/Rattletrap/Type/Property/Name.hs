module Rattletrap.Type.Property.Name where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Vendor.Argo as Argo

newtype Name
  = Name Str.Str
  deriving (Eq, Show)

instance Argo.HasCodec Name where
  codec = Argo.identified $ Argo.map fromStr toStr Argo.codec

fromStr :: Str.Str -> Name
fromStr = Name

toStr :: Name -> Str.Str
toStr (Name x) = x

bytePut :: Name -> BytePut.BytePut
bytePut = Str.bytePut . toStr

byteGet :: ByteGet.ByteGet Name
byteGet = ByteGet.label "Name" $ fmap fromStr Str.byteGet
