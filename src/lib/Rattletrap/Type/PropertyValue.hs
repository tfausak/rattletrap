{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PropertyValue where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.Float32le as Float32le
import qualified Rattletrap.Type.Int32le as Int32le
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word64le as Word64le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data PropertyValue a
  = Array (List.List (Dictionary.Dictionary a))
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | Bool Word8le.Word8le
  | Byte Str.Str (Maybe Str.Str)
  -- ^ This is a strange name for essentially a key-value pair.
  | Float Float32le.Float32le
  | Int Int32le.Int32le
  | Name Str.Str
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | QWord Word64le.Word64le
  | Str Str.Str
  deriving (Eq, Show)

$(deriveJsonWith ''PropertyValue jsonOptions)

bytePut :: (a -> BytePut) -> PropertyValue a -> BytePut
bytePut putProperty value = case value of
  Array list -> List.bytePut (Dictionary.bytePut putProperty) list
  Bool word8 -> Word8le.bytePut word8
  Byte k mv -> do
    Str.bytePut k
    case mv of
      Nothing -> pure ()
      Just v -> Str.bytePut v
  Float float32 -> Float32le.bytePut float32
  Int int32 -> Int32le.bytePut int32
  Name text -> Str.bytePut text
  QWord word64 -> Word64le.bytePut word64
  Str text -> Str.bytePut text

byteGet :: ByteGet a -> Str.Str -> ByteGet (PropertyValue a)
byteGet getProperty kind = case Str.toString kind of
  "ArrayProperty" ->
    Array <$> List.byteGet (Dictionary.byteGet getProperty)
  "BoolProperty" -> Bool <$> Word8le.byteGet
  "ByteProperty" -> do
    k <- Str.byteGet
    Byte k
      <$> decodeWhen (Str.toString k /= "OnlinePlatform_Steam") Str.byteGet
  "FloatProperty" -> Float <$> Float32le.byteGet
  "IntProperty" -> Int <$> Int32le.byteGet
  "NameProperty" -> Name <$> Str.byteGet
  "QWordProperty" -> QWord <$> Word64le.byteGet
  "StrProperty" -> Str <$> Str.byteGet
  _ -> fail ("[RT07] don't know how to read property value " <> show kind)
