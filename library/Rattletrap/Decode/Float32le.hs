module Rattletrap.Decode.Float32le
  ( decodeFloat32le
  , decodeFloat32leBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.Float32le

decodeFloat32le :: Decode Float32le
decodeFloat32le = Float32le <$> getFloatle

decodeFloat32leBits :: DecodeBits Float32le
decodeFloat32leBits = toBits decodeFloat32le 4
