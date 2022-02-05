module Rattletrap.Type.Attribute.Int64 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I64 as I64
import qualified Rattletrap.Vendor.Argo as Argo

newtype Int64A = Int64 -- TODO
  { value :: I64.I64
  } deriving (Eq, Show)

instance Argo.HasCodec Int64A where
  codec = Argo.identified $ Argo.map Int64 value Argo.codec

putInt64Attribute :: Int64A -> BitPut.BitPut
putInt64Attribute int64Attribute = I64.bitPut (value int64Attribute)

bitGet :: BitGet.BitGet Int64A
bitGet = BitGet.label "Int64" $ do
  value <- BitGet.label "value" I64.bitGet
  pure Int64 { value }
