module Rattletrap.Decode.FlaggedIntAttribute
  ( decodeFlaggedIntAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Type.FlaggedIntAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodeFlaggedIntAttributeBits :: DecodeBits FlaggedIntAttribute
decodeFlaggedIntAttributeBits =
  FlaggedIntAttribute <$> BinaryBits.getBool <*> decodeInt32leBits
