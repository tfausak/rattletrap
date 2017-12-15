module Rattletrap.Decode.Property
  ( getProperty
  ) where

import Rattletrap.Decode.PropertyValue
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word64le
import Rattletrap.Type.Property

import qualified Data.Binary as Binary

getProperty :: Binary.Get Property
getProperty = do
  kind <- getText
  size <- getWord64
  value <- getPropertyValue getProperty kind
  pure (Property kind size value)
