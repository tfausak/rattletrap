module Rattletrap.Type.Attribute.Int where

import Prelude hiding (Int)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Vendor.Argo as Argo

newtype IntA = Int -- TODO
  { value :: I32.I32
  } deriving (Eq, Show)

instance Argo.HasCodec IntA where
  codec = Argo.identified $ Argo.map Int value Argo.codec

bitPut :: IntA -> BitPut.BitPut
bitPut intAttribute = I32.bitPut (value intAttribute)

bitGet :: BitGet.BitGet IntA
bitGet = BitGet.label "Int" $ do
  value <- BitGet.label "value" I32.bitGet
  pure Int { value }
