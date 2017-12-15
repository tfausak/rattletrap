module Rattletrap.Encode.Property
  ( putProperty
  ) where

import Rattletrap.Type.Property
import Rattletrap.Encode.Text
import Rattletrap.Encode.Word64le
import Rattletrap.Encode.PropertyValue

import qualified Data.Binary as Binary

putProperty :: Property -> Binary.Put
putProperty property = do
  putText (propertyKind property)
  putWord64 (propertySize property)
  putPropertyValue putProperty (propertyValue property)
