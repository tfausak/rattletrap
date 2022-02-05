module Rattletrap.Type.Property.Bool where

import Prelude hiding (Bool)
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Vendor.Argo as Argo

newtype BoolP -- TODO
  = Bool U8.U8
  deriving (Eq, Show)

instance Argo.HasCodec BoolP where
  codec = Argo.identified $ Argo.map fromU8 toU8 Argo.codec

fromU8 :: U8.U8 -> BoolP
fromU8 = Bool

toU8 :: BoolP -> U8.U8
toU8 (Bool x) = x

bytePut :: BoolP -> BytePut.BytePut
bytePut = U8.bytePut . toU8

byteGet :: ByteGet.ByteGet BoolP
byteGet = ByteGet.label "Bool" $ fmap fromU8 U8.byteGet
