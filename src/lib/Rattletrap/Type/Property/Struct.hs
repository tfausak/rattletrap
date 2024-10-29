module Rattletrap.Type.Property.Struct where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

data Struct a = Struct
  { name :: Str.Str,
    fields :: Dictionary.Dictionary a
  }
  deriving (Eq, Show)

instance (Json.FromJSON a) => Json.FromJSON (Struct a) where
  parseJSON = Json.withObject "Struct" $ \o -> do
    name <- Json.required o "name"
    fields <- Json.required o "fields"
    pure Struct {name, fields}

instance (Json.ToJSON a) => Json.ToJSON (Struct a) where
  toJSON x =
    Json.object
      [ Json.pair "name" $ name x,
        Json.pair "fields" $ fields x
      ]

schema :: Schema.Schema -> Schema.Schema
schema s =
  Schema.named "property-struct" $
    Schema.object
      [ (Json.pair "name" $ Schema.ref Str.schema, True),
        (Json.pair "fields" $ Schema.ref (Dictionary.schema s), True)
      ]

bytePut :: (a -> BytePut.BytePut) -> Struct a -> BytePut.BytePut
bytePut p x =
  Str.bytePut (name x)
    <> Dictionary.bytePut p (fields x)

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (Struct a)
byteGet g = ByteGet.label "Struct" $ do
  name <- ByteGet.label "name" Str.byteGet
  fields <- ByteGet.label "fields" $ Dictionary.byteGet g
  pure Struct {name, fields}
