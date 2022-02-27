module Rattletrap.Type.PropertyValue where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Exception.UnknownProperty as UnknownProperty
import qualified Rattletrap.Type.Property.Array as Property.Array
import qualified Rattletrap.Type.Property.Bool as Property.Bool
import qualified Rattletrap.Type.Property.Byte as Property.Byte
import qualified Rattletrap.Type.Property.Float as Property.Float
import qualified Rattletrap.Type.Property.Int as Property.Int
import qualified Rattletrap.Type.Property.Name as Property.Name
import qualified Rattletrap.Type.Property.QWord as Property.QWord
import qualified Rattletrap.Type.Property.Str as Property.Str
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Vendor.Argo as Argo

data PropertyValue a
  = Array (Property.Array.Array a)
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | Bool Property.Bool.Bool
  | Byte Property.Byte.Byte
  -- ^ This is a strange name for essentially a key-value pair.
  | Float Property.Float.Float
  | Int Property.Int.Int
  | Name Property.Name.Name
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | QWord Property.QWord.QWord
  | Str Property.Str.Str
  deriving (Eq, Show)

instance Argo.HasCodec a => Argo.HasCodec (PropertyValue a) where
  codec =
    Argo.identified
      $ Argo.mapMaybe
          (Just . Array)
          (\x -> case x of
            Array y -> Just y
            _ -> Nothing
          )
          (Argo.fromObjectCodec Argo.Allow (Argo.required "array" Argo.codec))
      Argo.<|> Argo.mapMaybe
                 (Just . Bool)
                 (\x -> case x of
                   Bool y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required "bool" Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Byte)
                 (\x -> case x of
                   Byte y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required "byte" Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Float)
                 (\x -> case x of
                   Float y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required "float" Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Int)
                 (\x -> case x of
                   Int y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required "int" Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Name)
                 (\x -> case x of
                   Name y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required "name" Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . QWord)
                 (\x -> case x of
                   QWord y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required "q_word" Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Str)
                 (\x -> case x of
                   Str y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required "str" Argo.codec)
                 )

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
    x -> ByteGet.throw $ UnknownProperty.UnknownProperty x
