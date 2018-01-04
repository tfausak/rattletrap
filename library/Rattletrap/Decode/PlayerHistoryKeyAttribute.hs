module Rattletrap.Decode.PlayerHistoryKeyAttribute
  ( decodePlayerHistoryKeyAttributeBits
  ) where

import Rattletrap.Decode.Bitstream
import Rattletrap.Decode.Common
import Rattletrap.Type.PlayerHistoryKeyAttribute

decodePlayerHistoryKeyAttributeBits :: DecodeBits PlayerHistoryKeyAttribute
decodePlayerHistoryKeyAttributeBits =
  PlayerHistoryKeyAttribute <$> decodeBitstreamBits 1253
