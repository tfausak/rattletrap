module Rattletrap.Decode.PickupAttribute
  ( decodePickupAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Type.PickupAttribute

import qualified Data.Binary.Bits.Get as BinaryBits

decodePickupAttributeBits :: DecodeBits PickupAttribute
decodePickupAttributeBits = do
  instigator <- BinaryBits.getBool
  PickupAttribute
    <$> decodeWhen instigator decodeWord32leBits
    <*> BinaryBits.getBool
