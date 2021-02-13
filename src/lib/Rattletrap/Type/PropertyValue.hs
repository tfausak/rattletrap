{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PropertyValue where

import Rattletrap.Type.Common
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Float32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word64le as Word64le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data PropertyValue a
  = Array (List (Dictionary a))
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | Bool Word8le.Word8le
  | Byte Str.Str (Maybe Str.Str)
  -- ^ This is a strange name for essentially a key-value pair.
  | Float Float32le
  | Int Int32le
  | Name Str.Str
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | QWord Word64le.Word64le
  | Str Str.Str
  deriving (Eq, Show)

$(deriveJsonWith ''PropertyValue jsonOptions)

bytePut :: (a -> BytePut) -> PropertyValue a -> BytePut
bytePut putProperty value = case value of
  Array list -> putList (putDictionary putProperty) list
  Bool word8 -> Word8le.bytePut word8
  Byte k mv -> do
    Str.bytePut k
    case mv of
      Nothing -> pure ()
      Just v -> Str.bytePut v
  Float float32 -> putFloat32 float32
  Int int32 -> putInt32 int32
  Name text -> Str.bytePut text
  QWord word64 -> Word64le.bytePut word64
  Str text -> Str.bytePut text

byteGet :: ByteGet a -> Str.Str -> ByteGet (PropertyValue a)
byteGet getProperty kind = case Str.toString kind of
  "ArrayProperty" ->
    Array <$> decodeList (decodeDictionary getProperty)
  "BoolProperty" -> Bool <$> Word8le.byteGet
  "ByteProperty" -> do
    k <- Str.byteGet
    Byte k
      <$> decodeWhen (Str.toString k /= "OnlinePlatform_Steam") Str.byteGet
  "FloatProperty" -> Float <$> decodeFloat32le
  "IntProperty" -> Int <$> decodeInt32le
  "NameProperty" -> Name <$> Str.byteGet
  "QWordProperty" -> QWord <$> Word64le.byteGet
  "StrProperty" -> Str <$> Str.byteGet
  _ -> fail ("[RT07] don't know how to read property value " <> show kind)
