module Rattletrap.Type.Property where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

data Property = Property
  { kind :: Str.Str,
    -- | Not used.
    size :: U32.U32,
    index :: U32.U32,
    value :: PropertyValue.PropertyValue Property
  }
  deriving (Eq, Show)

instance Json.FromJSON Property where
  parseJSON = Json.withObject "Property" $ \object -> do
    kind <- Json.required object "kind"
    size <- Json.required object "size"
    index <- Json.required object "index"
    value <- Json.required object "value"
    pure Property {kind, size, index, value}

instance Json.ToJSON Property where
  toJSON x =
    Json.object
      [ Json.pair "kind" $ kind x,
        Json.pair "size" $ size x,
        Json.pair "index" $ index x,
        Json.pair "value" $ value x
      ]

schema :: Schema.Schema
schema =
  Schema.named "property" $
    Schema.object
      [ (Json.pair "kind" $ Schema.ref Str.schema, True),
        (Json.pair "size" $ Schema.ref U32.schema, True),
        (Json.pair "index" $ Schema.ref U32.schema, True),
        (Json.pair "value" . Schema.ref $ PropertyValue.schema schema, True)
      ]

bytePut :: Property -> BytePut.BytePut
bytePut x =
  Str.bytePut (kind x)
    <> U32.bytePut (size x)
    <> U32.bytePut (index x)
    <> PropertyValue.bytePut
      bytePut
      (value x)

byteGet :: ByteGet.ByteGet Property
byteGet = ByteGet.label "Property" $ do
  kind <- ByteGet.label "kind" Str.byteGet
  size <- ByteGet.label "size" U32.byteGet
  index <- ByteGet.label "index" U32.byteGet
  value <- ByteGet.label "value" $ PropertyValue.byteGet byteGet kind
  pure Property {kind, size, index, value}
