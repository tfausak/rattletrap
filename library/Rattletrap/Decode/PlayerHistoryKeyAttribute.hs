module Rattletrap.Decode.PlayerHistoryKeyAttribute
  ( decodePlayerHistoryKeyAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.PlayerHistoryKeyAttribute

import qualified Control.Monad as Monad

decodePlayerHistoryKeyAttributeBits :: DecodeBits PlayerHistoryKeyAttribute
decodePlayerHistoryKeyAttributeBits = (PlayerHistoryKeyAttribute . Bits) <$> Monad.replicateM 1253 getBool
