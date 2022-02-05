module Rattletrap.Type.Attribute.Boolean where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Vendor.Argo as Argo

newtype Boolean = Boolean
  { value :: Bool
  } deriving (Eq, Show)

instance Argo.HasCodec Boolean where
  codec = Argo.identified $ Argo.map Boolean value Argo.codec

bitPut :: Boolean -> BitPut.BitPut
bitPut booleanAttribute = BitPut.bool (value booleanAttribute)

bitGet :: BitGet.BitGet Boolean
bitGet = BitGet.label "Boolean" $ do
  value <- BitGet.label "value" BitGet.bool
  pure Boolean { value }
