module Rattletrap.Decode.ClassMapping
  ( decodeClassMapping
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.ClassMapping

decodeClassMapping :: Decode ClassMapping
decodeClassMapping = ClassMapping <$> getText <*> getWord32
