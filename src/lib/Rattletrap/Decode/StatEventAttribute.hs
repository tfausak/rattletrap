module Rattletrap.Decode.StatEventAttribute
  ( decodeStatEventAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Int32le
import Rattletrap.Type.StatEventAttribute

decodeStatEventAttributeBits :: DecodeBits StatEventAttribute
decodeStatEventAttributeBits =
  StatEventAttribute <$> getBool <*> decodeInt32leBits
