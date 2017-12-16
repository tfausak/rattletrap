module Rattletrap.Decode.Float32le
  ( decodeFloat32le
  , decodeFloat32leBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.Float32le

import qualified Data.Binary.Get as Binary

decodeFloat32le :: Decode Float32le
decodeFloat32le = Float32le <$> Binary.getFloatle

decodeFloat32leBits :: DecodeBits Float32le
decodeFloat32leBits = toBits decodeFloat32le 4
