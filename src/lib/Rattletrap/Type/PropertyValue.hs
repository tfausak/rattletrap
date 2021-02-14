{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PropertyValue where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Utility.Monad
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.ByteGet as ByteGet

data PropertyValue a
  = Array (List.List (Dictionary.Dictionary a))
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | Bool U8.U8
  | Byte Str.Str (Maybe Str.Str)
  -- ^ This is a strange name for essentially a key-value pair.
  | Float F32.F32
  | Int I32.I32
  | Name Str.Str
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | QWord U64.U64
  | Str Str.Str
  deriving (Eq, Show)

$(deriveJson ''PropertyValue)

bytePut :: (a -> BytePut.BytePut) -> PropertyValue a -> BytePut.BytePut
bytePut putProperty value = case value of
  Array list -> List.bytePut (Dictionary.bytePut putProperty) list
  Bool word8 -> U8.bytePut word8
  Byte k mv -> do
    Str.bytePut k
    case mv of
      Nothing -> pure ()
      Just v -> Str.bytePut v
  Float float32 -> F32.bytePut float32
  Int int32 -> I32.bytePut int32
  Name text -> Str.bytePut text
  QWord word64 -> U64.bytePut word64
  Str text -> Str.bytePut text

byteGet :: ByteGet.ByteGet a -> Str.Str -> ByteGet.ByteGet (PropertyValue a)
byteGet getProperty kind = case Str.toString kind of
  "ArrayProperty" ->
    Array <$> List.byteGet (Dictionary.byteGet getProperty)
  "BoolProperty" -> Bool <$> U8.byteGet
  "ByteProperty" -> do
    k <- Str.byteGet
    Byte k
      <$> whenMaybe (Str.toString k /= "OnlinePlatform_Steam") Str.byteGet
  "FloatProperty" -> Float <$> F32.byteGet
  "IntProperty" -> Int <$> I32.byteGet
  "NameProperty" -> Name <$> Str.byteGet
  "QWordProperty" -> QWord <$> U64.byteGet
  "StrProperty" -> Str <$> Str.byteGet
  _ -> fail ("[RT07] don't know how to read property value " <> show kind)
