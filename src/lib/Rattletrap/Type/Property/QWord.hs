module Rattletrap.Type.Property.QWord where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Vendor.Argo as Argo

newtype QWord
  = QWord U64.U64
  deriving (Eq, Show)

instance Argo.HasCodec QWord where
  codec = Argo.identified $ Argo.map fromU64 toU64 Argo.codec

fromU64 :: U64.U64 -> QWord
fromU64 = QWord

toU64 :: QWord -> U64.U64
toU64 (QWord x) = x

bytePut :: QWord -> BytePut.BytePut
bytePut = U64.bytePut . toU64

byteGet :: ByteGet.ByteGet QWord
byteGet = ByteGet.label "QWord" $ fmap fromU64 U64.byteGet
