module Rattletrap.Decode.GameModeAttribute
  ( getGameModeAttribute
  ) where

import Rattletrap.Type.GameModeAttribute

import qualified Data.Binary.Bits.Get as BinaryBit

getGameModeAttribute :: (Int, Int, Int) -> BinaryBit.BitGet GameModeAttribute
getGameModeAttribute version = do
  let numBits = if version < (868, 12, 0) then 2 else 8 :: Int
  word <- BinaryBit.getWord8 numBits
  pure (GameModeAttribute numBits word)
