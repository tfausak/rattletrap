module Rattletrap.Type.Attribute.QWord where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Vendor.Argo as Argo

newtype QWord = QWord
  { value :: U64.U64
  } deriving (Eq, Show)

instance Argo.HasCodec QWord where
  codec = Argo.identified $ Argo.map QWord value Argo.codec

bitPut :: QWord -> BitPut.BitPut
bitPut qWordAttribute = U64.bitPut (value qWordAttribute)

bitGet :: BitGet.BitGet QWord
bitGet = BitGet.label "QWord" $ do
  value <- BitGet.label "value" U64.bitGet
  pure QWord { value }
