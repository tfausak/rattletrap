module Rattletrap.Decode.FloatAttribute
  ( decodeFloatAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Float32le
import Rattletrap.Type.FloatAttribute

decodeFloatAttributeBits :: DecodeBits FloatAttribute
decodeFloatAttributeBits = FloatAttribute <$> decodeFloat32leBits
