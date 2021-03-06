module Rattletrap.Type.PropertyValue where

import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.Property.Int as Property.Int
import qualified Rattletrap.Type.Property.Name as Property.Name
import qualified Rattletrap.Type.Property.QWord as Property.QWord
import qualified Rattletrap.Type.Property.Str as Property.Str
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json
import Rattletrap.Utility.Monad

data PropertyValue a
  = Array (List.List (Dictionary.Dictionary a))
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | Bool U8.U8
  | Byte Str.Str (Maybe Str.Str)
  -- ^ This is a strange name for essentially a key-value pair.
  | Float F32.F32
  | Int Property.Int.Int
  | Name Property.Name.Name
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | QWord Property.QWord.QWord
  | Str Property.Str.Str
  deriving (Eq, Show)

instance Json.FromJSON a => Json.FromJSON (PropertyValue a) where
  parseJSON = Json.withObject "PropertyValue" $ \object -> Foldable.asum
    [ fmap Array $ Json.required object "array"
    , fmap Bool $ Json.required object "bool"
    , fmap (uncurry Byte) $ Json.required object "byte"
    , fmap Float $ Json.required object "float"
    , fmap Int $ Json.required object "int"
    , fmap Name $ Json.required object "name"
    , fmap QWord $ Json.required object "q_word"
    , fmap Str $ Json.required object "str"
    ]

instance Json.ToJSON a => Json.ToJSON (PropertyValue a) where
  toJSON x = case x of
    Array y -> Json.object [Json.pair "array" y]
    Bool y -> Json.object [Json.pair "bool" y]
    Byte y z -> Json.object [Json.pair "byte" (y, z)]
    Float y -> Json.object [Json.pair "float" y]
    Int y -> Json.object [Json.pair "int" y]
    Name y -> Json.object [Json.pair "name" y]
    QWord y -> Json.object [Json.pair "q_word" y]
    Str y -> Json.object [Json.pair "str" y]

schema :: Schema.Schema -> Schema.Schema
schema s =
  Schema.named ("property-value-" <> Text.unpack (Schema.name s))
    . Schema.oneOf
    $ fmap
        (\(k, v) -> Schema.object [(Json.pair k v, True)])
        [ ("array", Schema.json . List.schema $ Dictionary.schema s)
        , ("bool", Schema.ref U8.schema)
        , ( "byte"
          , Schema.tuple
            [Schema.ref Str.schema, Schema.json $ Schema.maybe Str.schema]
          )
        , ("float", Schema.ref F32.schema)
        , ("int", Schema.ref Property.Int.schema)
        , ("name", Schema.ref Property.Name.schema)
        , ("q_word", Schema.ref Property.QWord.schema)
        , ("str", Schema.ref Property.Str.schema)
        ]

bytePut :: (a -> BytePut.BytePut) -> PropertyValue a -> BytePut.BytePut
bytePut putProperty value = case value of
  Array x -> List.bytePut (Dictionary.bytePut putProperty) x
  Bool x -> U8.bytePut x
  Byte k mv -> Str.bytePut k <> foldMap Str.bytePut mv
  Float x -> F32.bytePut x
  Int x -> Property.Int.bytePut x
  Name x -> Property.Name.bytePut x
  QWord x -> Property.QWord.bytePut x
  Str x -> Property.Str.bytePut x

byteGet :: ByteGet.ByteGet a -> Str.Str -> ByteGet.ByteGet (PropertyValue a)
byteGet getProperty kind = case Str.toString kind of
  "ArrayProperty" ->
    fmap Array $ List.byteGet (Dictionary.byteGet getProperty)
  "BoolProperty" -> fmap Bool U8.byteGet
  "ByteProperty" -> do
    k <- Str.byteGet
    v <- whenMaybe (Str.toString k /= "OnlinePlatform_Steam") Str.byteGet
    pure $ Byte k v
  "FloatProperty" -> fmap Float F32.byteGet
  "IntProperty" -> fmap Int Property.Int.byteGet
  "NameProperty" -> fmap Name Property.Name.byteGet
  "QWordProperty" -> fmap QWord Property.QWord.byteGet
  "StrProperty" -> fmap Str Property.Str.byteGet
  _ -> fail ("[RT07] don't know how to read property value " <> show kind)
