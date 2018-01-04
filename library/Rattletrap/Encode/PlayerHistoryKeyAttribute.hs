module Rattletrap.Encode.PlayerHistoryKeyAttribute
  ( putPlayerHistoryKeyAttribute
  ) where

import Rattletrap.Encode.Bitstream
import Rattletrap.Type.PlayerHistoryKeyAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putPlayerHistoryKeyAttribute :: PlayerHistoryKeyAttribute -> BinaryBits.BitPut ()
putPlayerHistoryKeyAttribute = putBitstream . playerHistoryKeyAttributeUnknown
