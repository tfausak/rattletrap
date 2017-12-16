module Rattletrap.Decode.FlaggedIntAttribute
  ( decodeFlaggedIntAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Type.FlaggedIntAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

decodeFlaggedIntAttributeBits :: DecodeBits FlaggedIntAttribute
decodeFlaggedIntAttributeBits =
  FlaggedIntAttribute <$> BinaryBit.getBool <*> getInt32Bits
