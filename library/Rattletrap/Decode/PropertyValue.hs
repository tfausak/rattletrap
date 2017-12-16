module Rattletrap.Decode.PropertyValue
  ( getPropertyValue
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Decode.Dictionary
import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.List
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word64le
import Rattletrap.Decode.Word8le
import Rattletrap.Type.PropertyValue
import Rattletrap.Type.Str

import qualified Data.Binary as Binary

getPropertyValue :: Binary.Get a -> Str -> Binary.Get (PropertyValue a)
getPropertyValue getProperty kind = case fromStr kind of
  "ArrayProperty" -> do
    list <- getList (decodeDictionary getProperty)
    pure (PropertyValueArray list)
  "BoolProperty" -> do
    word8 <- getWord8
    pure (PropertyValueBool word8)
  "ByteProperty" -> do
    k <- getText
    v <- if fromStr k == "OnlinePlatform_Steam"
      then pure Nothing
      else do
        v <- getText
        pure (Just v)
    pure (PropertyValueByte k v)
  "FloatProperty" -> do
    float32 <- decodeFloat32le
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
  _ -> fail ("don't know how to read property value " <> show kind)
