module Rattletrap.Decode.PropertyValue
  ( getPropertyValue
  ) where

import Rattletrap.Type.PropertyValue
import Rattletrap.Type.Text
import Rattletrap.Decode.List
import Rattletrap.Decode.Dictionary
import Rattletrap.Decode.Word8le
import Rattletrap.Decode.Text
import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Word64

import qualified Data.Binary as Binary

getPropertyValue :: Binary.Get a -> Text -> Binary.Get (PropertyValue a)
getPropertyValue getProperty kind = case textToString kind of
  "ArrayProperty" -> do
    list <- getList (getDictionary getProperty)
    pure (PropertyValueArray list)
  "BoolProperty" -> do
    word8 <- getWord8
    pure (PropertyValueBool word8)
  "ByteProperty" -> do
    k <- getText
    v <- if textToString k == "OnlinePlatform_Steam"
      then pure Nothing
      else do
        v <- getText
        pure (Just v)
    pure (PropertyValueByte k v)
  "FloatProperty" -> do
    float32 <- getFloat32
    pure (PropertyValueFloat float32)
  "IntProperty" -> do
    int32 <- getInt32
    pure (PropertyValueInt int32)
  "NameProperty" -> do
    text <- getText
    pure (PropertyValueName text)
  "QWordProperty" -> do
    word64 <- getWord64
    pure (PropertyValueQWord word64)
  "StrProperty" -> do
    text <- getText
    pure (PropertyValueStr text)
  _ -> fail ("don't know how to read property value " ++ show kind)
