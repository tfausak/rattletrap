module Rattletrap.Decode.Int8le
  ( decodeInt8le
  , decodeInt8leBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.Int8le

import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Binary.Get as Binary

decodeInt8le :: Binary.Get Int8le
decodeInt8le = Int8le <$> Binary.getInt8

decodeInt8leBits :: BinaryBits.BitGet Int8le
decodeInt8leBits = toBits decodeInt8le 1
