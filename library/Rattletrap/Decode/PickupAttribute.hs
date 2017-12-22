module Rattletrap.Decode.PickupAttribute
  ( decodePickupAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Type.PickupAttribute

decodePickupAttributeBits :: DecodeBits PickupAttribute
decodePickupAttributeBits = do
  instigator <- getBool
  PickupAttribute <$> decodeWhen instigator decodeWord32leBits <*> getBool
