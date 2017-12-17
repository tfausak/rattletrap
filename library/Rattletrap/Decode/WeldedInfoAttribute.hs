module Rattletrap.Decode.WeldedInfoAttribute
  ( decodeWeldedInfoAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Int32le
import Rattletrap.Decode.Int8Vector
import Rattletrap.Decode.Vector
import Rattletrap.Type.WeldedInfoAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeWeldedInfoAttributeBits :: DecodeBits WeldedInfoAttribute
decodeWeldedInfoAttributeBits =
  WeldedInfoAttribute
    <$> BinaryBits.getBool
    <*> decodeInt32leBits
    <*> decodeVectorBits
    <*> decodeFloat32leBits
    <*> decodeInt8VectorBits
