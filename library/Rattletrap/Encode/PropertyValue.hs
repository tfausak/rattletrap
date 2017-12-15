module Rattletrap.Encode.PropertyValue
  ( putPropertyValue
  ) where

import Rattletrap.Type.PropertyValue
import Rattletrap.Encode.List
import Rattletrap.Encode.Dictionary
import Rattletrap.Encode.Word8le
import Rattletrap.Encode.Text
import Rattletrap.Encode.Float32le
import Rattletrap.Encode.Int32le
import Rattletrap.Encode.Word64le

import qualified Data.Binary as Binary

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
