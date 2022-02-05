module Rattletrap.Type.Attribute.PlayerHistoryKey where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Vendor.Argo as Argo

newtype PlayerHistoryKey = PlayerHistoryKey
  { unknown :: Word.Word16
  } deriving (Eq, Show)

instance Argo.HasCodec PlayerHistoryKey where
  codec = Argo.identified $ Argo.map PlayerHistoryKey unknown Argo.codec

bitPut :: PlayerHistoryKey -> BitPut.BitPut
bitPut = BitPut.bits 14 . unknown

bitGet :: BitGet.BitGet PlayerHistoryKey
bitGet = BitGet.label "PlayerHistoryKey" $ do
  unknown <- BitGet.label "unknown" $ BitGet.bits 14
  pure PlayerHistoryKey { unknown }
