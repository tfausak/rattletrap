module Rattletrap.Decode.FlaggedIntAttribute
  ( decodeFlaggedIntAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Type.FlaggedIntAttribute

decodeFlaggedIntAttributeBits :: DecodeBits FlaggedIntAttribute
decodeFlaggedIntAttributeBits =
  FlaggedIntAttribute <$> getBool <*> decodeInt32leBits
