module Rattletrap.Type.Property.Str where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Vendor.Argo as Argo

newtype StrP -- TODO
  = Str Str.Str
  deriving (Eq, Show)

instance Argo.HasCodec StrP where
  codec = Argo.identified $ Argo.map fromStr toStr Argo.codec

fromStr :: Str.Str -> StrP
fromStr = Str

toStr :: StrP -> Str.Str
toStr (Str x) = x

bytePut :: StrP -> BytePut.BytePut
bytePut = Str.bytePut . toStr

byteGet :: ByteGet.ByteGet StrP
byteGet = ByteGet.label "Str" $ fmap fromStr Str.byteGet
