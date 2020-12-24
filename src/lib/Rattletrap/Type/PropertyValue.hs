{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PropertyValue
  ( PropertyValue(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Float32le
import Rattletrap.Type.Int32le
import Rattletrap.Type.List
import Rattletrap.Type.Str
import Rattletrap.Type.Word64le
import Rattletrap.Type.Word8le

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
  deriving (Eq, Ord, Show)

$(deriveJson ''PropertyValue)
