module Rattletrap.Decode.PlayerHistoryKeyAttribute
  ( decodePlayerHistoryKeyAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.PlayerHistoryKeyAttribute

decodePlayerHistoryKeyAttributeBits :: DecodeBits PlayerHistoryKeyAttribute
decodePlayerHistoryKeyAttributeBits =
  PlayerHistoryKeyAttribute <$> getWord16be 14
