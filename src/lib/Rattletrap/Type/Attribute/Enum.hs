module Rattletrap.Type.Attribute.Enum where

import qualified Data.Word as Word
import Prelude hiding (Enum)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Vendor.Argo as Argo

newtype Enum = Enum
  { value :: Word.Word16
  } deriving (Eq, Show)

instance Argo.HasCodec Enum where
  codec = Argo.identified $ Argo.map Enum value Argo.codec

bitPut :: Enum -> BitPut.BitPut
bitPut enumAttribute = BitPut.bits 11 (value enumAttribute)

bitGet :: BitGet.BitGet Enum
bitGet = BitGet.label "Enum" $ do
  value <- BitGet.label "value" $ BitGet.bits 11
  pure Enum { value }
