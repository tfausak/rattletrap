module Rattletrap.Decode.Property
  ( decodeProperty
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.PropertyValue
import Rattletrap.Decode.Str
import Rattletrap.Type.Word64le
import Rattletrap.Type.Property

decodeProperty :: Decode Property
decodeProperty = do
  kind <- decodeStr
  Property kind <$> decodeWord64le <*> decodePropertyValue decodeProperty kind
