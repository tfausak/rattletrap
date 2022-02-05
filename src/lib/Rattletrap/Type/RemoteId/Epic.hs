module Rattletrap.Type.RemoteId.Epic where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Vendor.Argo as Argo

newtype Epic
  = Epic Str.Str
  deriving (Eq, Show)

instance Argo.HasCodec Epic where
  codec = Argo.identified $ Argo.map fromStr toStr Argo.codec

fromStr :: Str.Str -> Epic
fromStr = Epic

toStr :: Epic -> Str.Str
toStr (Epic x) = x

bitPut :: Epic -> BitPut.BitPut
bitPut = Str.bitPut . toStr

bitGet :: BitGet.BitGet Epic
bitGet = BitGet.label "Epic" $ fmap fromStr Str.bitGet
