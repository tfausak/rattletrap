module Rattletrap.Type.Property where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Utility.Json as Json

data Property = Property
  { kind :: Str.Str
  , size :: U64.U64
  -- ^ Not used.
  , value :: PropertyValue.PropertyValue Property
  }
  deriving (Eq, Show)

instance Json.FromJSON Property where
  parseJSON = Json.withObject "Property" $ \ object -> do
    kind <- Json.required object "kind"
    size <- Json.required object "size"
    value <- Json.required object "value"
    pure Property { kind, size, value }

instance Json.ToJSON Property where
  toJSON x = Json.object
    [ Json.pair "kind" $ kind x
    , Json.pair "size" $ size x
    , Json.pair "value" $ value x
    ]

schema :: Schema.Schema
schema = Schema.named "property" $ Schema.object
  [ (Json.pair "kind" $ Schema.ref Str.schema, True)
  , (Json.pair "size" $ Schema.ref U64.schema, True)
  , (Json.pair "value" . Schema.ref $ PropertyValue.schema schema, True)
  ]

bytePut :: Property -> BytePut.BytePut
bytePut x =
  Str.bytePut (kind x) <> U64.bytePut (size x) <> PropertyValue.bytePut
    bytePut
    (value x)

byteGet :: ByteGet.ByteGet Property
byteGet = do
  kind_ <- Str.byteGet
  Property kind_ <$> U64.byteGet <*> PropertyValue.byteGet byteGet kind_
