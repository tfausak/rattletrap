module Rattletrap.Type.PropertyValue where

import qualified Data.Foldable as Foldable
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Exception.UnknownProperty as UnknownProperty
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Property.Array as Property.Array
import qualified Rattletrap.Type.Property.Bool as Property.Bool
import qualified Rattletrap.Type.Property.Byte as Property.Byte
import qualified Rattletrap.Type.Property.Float as Property.Float
import qualified Rattletrap.Type.Property.Int as Property.Int
import qualified Rattletrap.Type.Property.Name as Property.Name
import qualified Rattletrap.Type.Property.QWord as Property.QWord
import qualified Rattletrap.Type.Property.Str as Property.Str
import qualified Rattletrap.Type.Property.Struct as Property.Struct
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

data PropertyValue a
  = -- | Yes, a list of dictionaries. No, it doesn't make sense. These usually
    -- only have one element.
    Array (Property.Array.Array a)
  | Bool Property.Bool.Bool
  | -- | This is a strange name for essentially a key-value pair.
    Byte Property.Byte.Byte
  | Float Property.Float.Float
  | Int Property.Int.Int
  | -- | It's unclear how exactly this is different than a 'StrProperty'.
    Name Property.Name.Name
  | QWord Property.QWord.QWord
  | Str Property.Str.Str
  | Struct (Property.Struct.Struct a)
  deriving (Eq, Show)

instance (Json.FromJSON a) => Json.FromJSON (PropertyValue a) where
  parseJSON = Json.withObject "PropertyValue" $ \object ->
    Foldable.asum
      [ fmap Array $ Json.required object "array",
        fmap Bool $ Json.required object "bool",
        fmap Byte $ Json.required object "byte",
        fmap Float $ Json.required object "float",
        fmap Int $ Json.required object "int",
        fmap Name $ Json.required object "name",
        fmap QWord $ Json.required object "q_word",
        fmap Str $ Json.required object "str",
        fmap Struct $ Json.required object "struct"
      ]

instance (Json.ToJSON a) => Json.ToJSON (PropertyValue a) where
  toJSON x = case x of
    Array y -> Json.object [Json.pair "array" y]
    Bool y -> Json.object [Json.pair "bool" y]
    Byte y -> Json.object [Json.pair "byte" y]
    Float y -> Json.object [Json.pair "float" y]
    Int y -> Json.object [Json.pair "int" y]
    Name y -> Json.object [Json.pair "name" y]
    QWord y -> Json.object [Json.pair "q_word" y]
    Str y -> Json.object [Json.pair "str" y]
    Struct y -> Json.object [Json.pair "struct" y]

schema :: Schema.Schema -> Schema.Schema
schema s =
  Schema.named "property-value" . Schema.oneOf $
    fmap
      (\(k, v) -> Schema.object [(Json.pair k v, True)])
      [ ("array", Schema.ref $ Property.Array.schema s),
        ("bool", Schema.ref Property.Bool.schema),
        ("byte", Schema.ref Property.Byte.schema),
        ("float", Schema.ref Property.Float.schema),
        ("int", Schema.ref Property.Int.schema),
        ("name", Schema.ref Property.Name.schema),
        ("q_word", Schema.ref Property.QWord.schema),
        ("str", Schema.ref Property.Str.schema),
        ("struct", Schema.ref $ Property.Struct.schema s)
      ]

bytePut :: (a -> BytePut.BytePut) -> PropertyValue a -> BytePut.BytePut
bytePut putProperty value = case value of
  Array x -> Property.Array.bytePut putProperty x
  Bool x -> Property.Bool.bytePut x
  Byte x -> Property.Byte.bytePut x
  Float x -> Property.Float.bytePut x
  Int x -> Property.Int.bytePut x
  Name x -> Property.Name.bytePut x
  QWord x -> Property.QWord.bytePut x
  Str x -> Property.Str.bytePut x
  Struct x -> Property.Struct.bytePut putProperty x

byteGet :: ByteGet.ByteGet a -> Str.Str -> ByteGet.ByteGet (PropertyValue a)
byteGet getProperty kind =
  ByteGet.label "PropertyValue" $ case Str.toString kind of
    "ArrayProperty" -> fmap Array $ Property.Array.byteGet getProperty
    "BoolProperty" -> fmap Bool Property.Bool.byteGet
    "ByteProperty" -> fmap Byte Property.Byte.byteGet
    "FloatProperty" -> fmap Float Property.Float.byteGet
    "IntProperty" -> fmap Int Property.Int.byteGet
    "NameProperty" -> fmap Name Property.Name.byteGet
    "QWordProperty" -> fmap QWord Property.QWord.byteGet
    "StrProperty" -> fmap Str Property.Str.byteGet
    "StructProperty" -> fmap Struct $ Property.Struct.byteGet getProperty
    x -> ByteGet.throw $ UnknownProperty.UnknownProperty x
