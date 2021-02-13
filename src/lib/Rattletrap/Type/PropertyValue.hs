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
  = PropertyValueArray (List (Dictionary a))
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | PropertyValueBool Word8le.Word8le
  | PropertyValueByte Str.Str (Maybe Str.Str)
  -- ^ This is a strange name for essentially a key-value pair.
  | PropertyValueFloat Float32le
  | PropertyValueInt Int32le
  | PropertyValueName Str.Str
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | PropertyValueQWord Word64le.Word64le
  | PropertyValueStr Str.Str
  deriving (Eq, Show)

$(deriveJson ''PropertyValue)

putPropertyValue :: (a -> BytePut) -> PropertyValue a -> BytePut
putPropertyValue putProperty value = case value of
  PropertyValueArray list -> putList (putDictionary putProperty) list
  PropertyValueBool word8 -> Word8le.bytePut word8
  PropertyValueByte k mv -> do
    Str.bytePut k
    case mv of
      Nothing -> pure ()
      Just v -> Str.bytePut v
  PropertyValueFloat float32 -> putFloat32 float32
  PropertyValueInt int32 -> putInt32 int32
  PropertyValueName text -> Str.bytePut text
  PropertyValueQWord word64 -> Word64le.bytePut word64
  PropertyValueStr text -> Str.bytePut text

decodePropertyValue :: ByteGet a -> Str.Str -> ByteGet (PropertyValue a)
decodePropertyValue getProperty kind = case Str.toString kind of
  "ArrayProperty" ->
    PropertyValueArray <$> decodeList (decodeDictionary getProperty)
  "BoolProperty" -> PropertyValueBool <$> Word8le.byteGet
  "ByteProperty" -> do
    k <- Str.byteGet
    PropertyValueByte k
      <$> decodeWhen (Str.toString k /= "OnlinePlatform_Steam") Str.byteGet
  "FloatProperty" -> PropertyValueFloat <$> decodeFloat32le
  "IntProperty" -> PropertyValueInt <$> decodeInt32le
  "NameProperty" -> PropertyValueName <$> Str.byteGet
  "QWordProperty" -> PropertyValueQWord <$> Word64le.byteGet
  "StrProperty" -> PropertyValueStr <$> Str.byteGet
  _ -> fail ("[RT07] don't know how to read property value " <> show kind)
