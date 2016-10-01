module Rattletrap.PropertyValue where

import Rattletrap.Dictionary
import Rattletrap.Float32
import Rattletrap.Int32
import Rattletrap.List
import Rattletrap.Text
import Rattletrap.Word64
import Rattletrap.Word8

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy.Char8 as ByteString

data PropertyValue a
  = ArrayProperty (List (Dictionary a))
  | BoolProperty Word8
  | ByteProperty Text
                 Text
  | FloatProperty Float32
  | IntProperty Int32
  | NameProperty Text
  | QWordProperty Word64
  | StrProperty Text
  deriving (Eq, Ord, Show)

getPropertyValue :: Binary.Get a
                 -> Text
                 -> Word64
                 -> Binary.Get (PropertyValue a)
getPropertyValue getProperty (Text _ kind) _size =
  case ByteString.unpack kind of
    "ArrayProperty\x00" -> do
      list <- getList (getDictionary getProperty)
      pure (ArrayProperty list)
    "BoolProperty\x00" -> do
      word8 <- getWord8
      pure (BoolProperty word8)
    "ByteProperty\x00" -> do
      k <- getText
      v <- getText
      pure (ByteProperty k v)
    "FloatProperty\x00" -> do
      float32 <- getFloat32
      pure (FloatProperty float32)
    "IntProperty\x00" -> do
      int32 <- getInt32
      pure (IntProperty int32)
    "NameProperty\x00" -> do
      text <- getText
      pure (NameProperty text)
    "QWordProperty\x00" -> do
      word64 <- getWord64
      pure (QWordProperty word64)
    "StrProperty\x00" -> do
      text <- getText
      pure (StrProperty text)
    _ -> fail ("don't know how to read property value " ++ show kind)

putPropertyValue :: (a -> Binary.Put) -> PropertyValue a -> Binary.Put
putPropertyValue putProperty value =
  case value of
    ArrayProperty list -> putList (putDictionary putProperty) list
    BoolProperty word8 -> putWord8 word8
    ByteProperty k v -> do
      putText k
      putText v
    FloatProperty float32 -> putFloat32 float32
    IntProperty int32 -> putInt32 int32
    NameProperty text -> putText text
    QWordProperty word64 -> putWord64 word64
    StrProperty text -> putText text
