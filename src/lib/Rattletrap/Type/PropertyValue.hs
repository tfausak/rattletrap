{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PropertyValue where

import Rattletrap.Type.Common
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Float32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.List
import Rattletrap.Type.Str
import Rattletrap.Type.Word64le
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary as Binary

data PropertyValue a
  = PropertyValueArray (List (Dictionary a))
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | PropertyValueBool Word8le
  | PropertyValueByte Str (Maybe Str)
  -- ^ This is a strange name for essentially a key-value pair.
  | PropertyValueFloat Float32le
  | PropertyValueInt Int32le
  | PropertyValueName Str
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | PropertyValueQWord Word64le
  | PropertyValueStr Str
  deriving (Eq, Show)

$(deriveJson ''PropertyValue)

putPropertyValue :: (a -> Binary.Put) -> PropertyValue a -> Binary.Put
putPropertyValue putProperty value = case value of
  PropertyValueArray list -> putList (putDictionary putProperty) list
  PropertyValueBool word8 -> putWord8 word8
  PropertyValueByte k mv -> do
    putText k
    case mv of
      Nothing -> pure ()
      Just v -> putText v
  PropertyValueFloat float32 -> putFloat32 float32
  PropertyValueInt int32 -> putInt32 int32
  PropertyValueName text -> putText text
  PropertyValueQWord word64 -> putWord64 word64
  PropertyValueStr text -> putText text

decodePropertyValue :: ByteGet a -> Str -> ByteGet (PropertyValue a)
decodePropertyValue getProperty kind = case fromStr kind of
  "ArrayProperty" ->
    PropertyValueArray <$> decodeList (decodeDictionary getProperty)
  "BoolProperty" -> PropertyValueBool <$> decodeWord8le
  "ByteProperty" -> do
    k <- decodeStr
    PropertyValueByte k
      <$> decodeWhen (fromStr k /= "OnlinePlatform_Steam") decodeStr
  "FloatProperty" -> PropertyValueFloat <$> decodeFloat32le
  "IntProperty" -> PropertyValueInt <$> decodeInt32le
  "NameProperty" -> PropertyValueName <$> decodeStr
  "QWordProperty" -> PropertyValueQWord <$> decodeWord64le
  "StrProperty" -> PropertyValueStr <$> decodeStr
  _ -> fail ("[RT07] don't know how to read property value " <> show kind)
