module Rattletrap.Decode.GameModeAttribute
  ( decodeGameModeAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.GameModeAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

decodeGameModeAttributeBits :: (Int, Int, Int) -> DecodeBits GameModeAttribute
decodeGameModeAttributeBits version =
  GameModeAttribute <$> pure (numBits version) <*> BinaryBit.getWord8
    (numBits version)

numBits :: (Int, Int, Int) -> Int
numBits version = if version < (868, 12, 0) then 2 else 8
