module Rattletrap.Decode.PlayerHistoryKeyAttribute
  ( decodePlayerHistoryKeyAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.PlayerHistoryKeyAttribute

import qualified Control.Monad as Monad

decodePlayerHistoryKeyAttributeBits :: DecodeBits PlayerHistoryKeyAttribute
decodePlayerHistoryKeyAttributeBits = PlayerHistoryKeyAttribute <$> Monad.replicateM 14 getBool

