module Rattletrap.Type.RemoteId.Switch where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Vendor.Argo as Argo

data Switch = Switch
  { a :: U64.U64
  , b :: U64.U64
  , c :: U64.U64
  , d :: U64.U64
  }
  deriving (Eq, Show)

instance Argo.HasCodec Switch where
  codec = Argo.identified $ Argo.map
    (\(a, b, c, d) -> Switch { a, b, c, d })
    (\Switch { a, b, c, d } -> (a, b, c, d))
    Argo.codec

bitPut :: Switch -> BitPut.BitPut
bitPut x =
  U64.bitPut (a x) <> U64.bitPut (b x) <> U64.bitPut (c x) <> U64.bitPut (d x)

bitGet :: BitGet.BitGet Switch
bitGet = BitGet.label "Switch" $ do
  a <- U64.bitGet
  b <- U64.bitGet
  c <- U64.bitGet
  d <- U64.bitGet
  pure Switch { a, b, c, d }
