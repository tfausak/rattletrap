module Rattletrap.Decode.Property
  ( getProperty
  ) where

import Rattletrap.Type.Property
import Rattletrap.Decode.Text
import Rattletrap.Decode.Word64le
import Rattletrap.Decode.PropertyValue

import qualified Data.Binary as Binary

getProperty :: Binary.Get Property
getProperty = do
  kind <- getText
  size <- getWord64
  value <- getPropertyValue getProperty kind
  pure (Property kind size value)
