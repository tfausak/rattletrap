module Rattletrap.Type.Property.Array where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Utility.Json as Json

newtype Array a
  = Array (List.List (Dictionary.Dictionary a))
  deriving (Eq, Show)

fromList :: List.List (Dictionary.Dictionary a) -> Array a
fromList = Array

toList :: Array a -> List.List (Dictionary.Dictionary a)
toList (Array x) = x

instance Json.FromJSON a => Json.FromJSON (Array a) where
  parseJSON = fmap fromList . Json.parseJSON

instance Json.ToJSON a => Json.ToJSON (Array a) where
  toJSON = Json.toJSON . toList

schema :: Schema.Schema -> Schema.Schema
schema s = Schema.named "property-array"
  . Schema.json . List.schema $ Dictionary.schema s

bytePut :: (a -> BytePut.BytePut) -> Array a -> BytePut.BytePut
bytePut f = List.bytePut (Dictionary.bytePut f) . toList

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (Array a)
byteGet f = fmap fromList $ List.byteGet (Dictionary.byteGet f)
