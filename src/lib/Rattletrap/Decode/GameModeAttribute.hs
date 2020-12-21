module Rattletrap.Decode.GameModeAttribute
  ( decodeGameModeAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.GameModeAttribute

decodeGameModeAttributeBits :: (Int, Int, Int) -> DecodeBits GameModeAttribute
decodeGameModeAttributeBits version =
  GameModeAttribute <$> pure (numBits version) <*> getWord8Bits
    (numBits version)

numBits :: (Int, Int, Int) -> Int
numBits version = if version >= (868, 12, 0) then 8 else 2
