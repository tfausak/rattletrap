module Rattletrap.Type.Property.Array where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Vendor.Argo as Argo

newtype Array a
  = Array (List.List (Dictionary.Dictionary a))
  deriving (Eq, Show)

instance Argo.HasCodec a => Argo.HasCodec (Array a) where
  codec = Argo.identified $ Argo.map fromList toList Argo.codec

fromList :: List.List (Dictionary.Dictionary a) -> Array a
fromList = Array

toList :: Array a -> List.List (Dictionary.Dictionary a)
toList (Array x) = x

bytePut :: (a -> BytePut.BytePut) -> Array a -> BytePut.BytePut
bytePut f = List.bytePut (Dictionary.bytePut f) . toList

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (Array a)
byteGet =
  ByteGet.label "Array" . fmap fromList . List.byteGet . Dictionary.byteGet
