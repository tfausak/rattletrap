module Rattletrap.Decode.AttributeMapping
  ( decodeAttributeMapping
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Type.AttributeMapping

decodeAttributeMapping :: Decode AttributeMapping
decodeAttributeMapping = AttributeMapping <$> getWord32 <*> getWord32
