{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PropertyValue
  ( PropertyValue(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.List
import Rattletrap.Type.Dictionary
import Rattletrap.Type.Word8
import Rattletrap.Type.Text
import Rattletrap.Type.Float32le
import Rattletrap.Type.Int32
import Rattletrap.Type.Word64

data PropertyValue a
  = PropertyValueArray (List (Dictionary a))
  -- ^ Yes, a list of dictionaries. No, it doesn't make sense. These usually
  -- only have one element.
  | PropertyValueBool Word8
  | PropertyValueByte Text (Maybe Text)
  -- ^ This is a strange name for essentially a key-value pair.
  | PropertyValueFloat Float32le
  | PropertyValueInt Int32
  | PropertyValueName Text
  -- ^ It's unclear how exactly this is different than a 'StrProperty'.
  | PropertyValueQWord Word64
  | PropertyValueStr Text
  deriving (Eq, Ord, Show)

$(deriveJson ''PropertyValue)
