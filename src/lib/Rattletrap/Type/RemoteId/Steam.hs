module Rattletrap.Type.RemoteId.Steam where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Vendor.Argo as Argo

newtype Steam
  = Steam U64.U64
  deriving (Eq, Show)

instance Argo.HasCodec Steam where
  codec = Argo.map fromU64 toU64 Argo.codec

fromU64 :: U64.U64 -> Steam
fromU64 = Steam

toU64 :: Steam -> U64.U64
toU64 (Steam x) = x

bitPut :: Steam -> BitPut.BitPut
bitPut = U64.bitPut . toU64

bitGet :: BitGet.BitGet Steam
bitGet = BitGet.label "Steam" $ fmap fromU64 U64.bitGet
