module Rattletrap.Decode.PickupAttributeNew
  ( decodePickupAttributeNewBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word8le
import Rattletrap.Decode.Word32le
import Rattletrap.Type.PickupAttributeNew

decodePickupAttributeNewBits :: DecodeBits PickupAttributeNew
decodePickupAttributeNewBits = do
  instigator <- getBool
  PickupAttributeNew <$> decodeWhen instigator decodeWord32leBits <*> decodeWord8leBits
