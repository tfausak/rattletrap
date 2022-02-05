module Rattletrap.Type.RemoteId.Xbox where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Vendor.Argo as Argo

newtype Xbox
  = Xbox U64.U64
  deriving (Eq, Show)

instance Argo.HasCodec Xbox where
  codec = Argo.identified $ Argo.map fromU64 toU64 Argo.codec

fromU64 :: U64.U64 -> Xbox
fromU64 = Xbox

toU64 :: Xbox -> U64.U64
toU64 (Xbox x) = x

bitPut :: Xbox -> BitPut.BitPut
bitPut = U64.bitPut . toU64

bitGet :: BitGet.BitGet Xbox
bitGet = BitGet.label "Xbox" $ fmap fromU64 U64.bitGet
