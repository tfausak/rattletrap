module Rattletrap.Decode.Int8Vector
  ( decodeInt8VectorBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int8le
import Rattletrap.Type.Int8Vector
import Rattletrap.Type.Int8le

import qualified Data.Binary.Bits.Get as BinaryBits

decodeInt8VectorBits :: DecodeBits Int8Vector
decodeInt8VectorBits =
  Int8Vector <$> decodeFieldBits <*> decodeFieldBits <*> decodeFieldBits

decodeFieldBits :: DecodeBits (Maybe Int8le)
decodeFieldBits = do
  hasField <- BinaryBits.getBool
  decodeWhen hasField decodeInt8leBits
