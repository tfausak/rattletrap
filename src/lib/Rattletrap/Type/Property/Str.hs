module Rattletrap.Type.Property.Str where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Vendor.Argo as Argo

newtype Str
  = Str Str.Str
  deriving (Eq, Show)

-- TODO: If this schema was identified, it would collide with the other `Str`.
instance Argo.HasCodec Str where
  codec = Argo.map fromStr toStr Argo.codec

fromStr :: Str.Str -> Str
fromStr = Str

toStr :: Str -> Str.Str
toStr (Str x) = x

bytePut :: Str -> BytePut.BytePut
bytePut = Str.bytePut . toStr

byteGet :: ByteGet.ByteGet Str
byteGet = ByteGet.label "Str" $ fmap fromStr Str.byteGet
