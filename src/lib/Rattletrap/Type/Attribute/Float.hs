module Rattletrap.Type.Attribute.Float where

import Prelude hiding (Float)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Vendor.Argo as Argo

newtype Float = Float
  { value :: F32.F32
  } deriving (Eq, Show)

instance Argo.HasCodec Float where
  codec = Argo.map Float value Argo.codec

bitPut :: Float -> BitPut.BitPut
bitPut floatAttribute = F32.bitPut (value floatAttribute)

bitGet :: BitGet.BitGet Float
bitGet = BitGet.label "Float" $ do
  value <- BitGet.label "value" F32.bitGet
  pure Float { value }
