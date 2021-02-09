module Rattletrap.Decode.PropertyValue
  ( decodePropertyValue
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Dictionary
import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.List
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word64le
import Rattletrap.Decode.Word8le
import Rattletrap.Type.PropertyValue
import Rattletrap.Type.Str

decodePropertyValue :: Decode a -> Str -> Decode (PropertyValue a)
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
