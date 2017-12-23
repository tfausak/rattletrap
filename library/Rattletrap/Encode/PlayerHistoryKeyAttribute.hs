module Rattletrap.Encode.PlayerHistoryKeyAttribute
  ( putPlayerHistoryKeyAttribute
  ) where

import Rattletrap.Type.PlayerHistoryKeyAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putPlayerHistoryKeyAttribute :: PlayerHistoryKeyAttribute -> BinaryBits.BitPut ()
putPlayerHistoryKeyAttribute = mapM_ BinaryBits.putBool . unwrapBits . playerHistoryKeyAttributeValue
