module Rattletrap.Type.Property.Int where

import Prelude hiding (Int)
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Vendor.Argo as Argo

newtype Int
  = Int I32.I32
  deriving (Eq, Show)

instance Argo.HasCodec Int where
  codec = Argo.identified $ Argo.map fromI32 toI32 Argo.codec

fromI32 :: I32.I32 -> Int
fromI32 = Int

toI32 :: Int -> I32.I32
toI32 (Int x) = x

bytePut :: Int -> BytePut.BytePut
bytePut = I32.bytePut . toI32

byteGet :: ByteGet.ByteGet Int
byteGet = ByteGet.label "I32" $ fmap fromI32 I32.byteGet
