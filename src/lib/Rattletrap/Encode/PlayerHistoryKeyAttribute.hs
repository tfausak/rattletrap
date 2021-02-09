module Rattletrap.Encode.PlayerHistoryKeyAttribute
  ( putPlayerHistoryKeyAttribute
  ) where

import Rattletrap.Encode.Common
import Rattletrap.Type.PlayerHistoryKeyAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putPlayerHistoryKeyAttribute
  :: PlayerHistoryKeyAttribute -> BinaryBits.BitPut ()
putPlayerHistoryKeyAttribute = putBitsLE 14 . playerHistoryKeyAttributeUnknown
