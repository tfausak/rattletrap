module Rattletrap.Type.Property.Bool where

import Prelude hiding (Bool)
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Vendor.Argo as Argo

newtype Bool
  = Bool U8.U8
  deriving (Eq, Show)

instance Argo.HasCodec Bool where
  codec = Argo.withIdentifier "BoolProperty" $ Argo.map fromU8 toU8 Argo.codec

fromU8 :: U8.U8 -> Bool
fromU8 = Bool

toU8 :: Bool -> U8.U8
toU8 (Bool x) = x

bytePut :: Bool -> BytePut.BytePut
bytePut = U8.bytePut . toU8

byteGet :: ByteGet.ByteGet Bool
byteGet = ByteGet.label "Bool" $ fmap fromU8 U8.byteGet
