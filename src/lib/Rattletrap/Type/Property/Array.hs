module Rattletrap.Type.Property.Array where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.List as RList
import qualified Rattletrap.Utility.Json as Json

newtype Array a
  = Array (RList.List (Dictionary.Dictionary a))
  deriving (Eq, Show)

fromList :: RList.List (Dictionary.Dictionary a) -> Array a
fromList = Array

toList :: Array a -> RList.List (Dictionary.Dictionary a)
toList (Array x) = x

instance (Json.FromJSON a) => Json.FromJSON (Array a) where
  parseJSON = fmap fromList . Json.parseJSON

instance (Json.ToJSON a) => Json.ToJSON (Array a) where
  toJSON = Json.toJSON . toList

schema :: Schema.Schema -> Schema.Schema
schema s =
  Schema.named "property-array" . Schema.json . RList.schema $
    Dictionary.schema
      s

bytePut :: (a -> BytePut.BytePut) -> Array a -> BytePut.BytePut
bytePut f = RList.bytePut (Dictionary.bytePut f) . toList

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (Array a)
byteGet =
  ByteGet.label "Array" . fmap fromList . RList.byteGet . Dictionary.byteGet
