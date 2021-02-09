module Rattletrap.Decode.PlayerHistoryKeyAttribute
  ( decodePlayerHistoryKeyAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.PlayerHistoryKeyAttribute

decodePlayerHistoryKeyAttributeBits :: DecodeBits PlayerHistoryKeyAttribute
decodePlayerHistoryKeyAttributeBits =
  PlayerHistoryKeyAttribute <$> getBitsLE 14
