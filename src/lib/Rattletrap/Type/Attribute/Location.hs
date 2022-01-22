module Rattletrap.Type.Attribute.Location where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

newtype Location = Location
  { value :: Vector.Vector
  } deriving (Eq, Show)

instance Argo.HasCodec Location where
  codec = Argo.map Location value Argo.codec

bitPut :: Location -> BitPut.BitPut
bitPut locationAttribute = Vector.bitPut (value locationAttribute)

bitGet :: Version.Version -> BitGet.BitGet Location
bitGet version = BitGet.label "Location" $ do
  value <- BitGet.label "value" $ Vector.bitGet version
  pure Location { value }
