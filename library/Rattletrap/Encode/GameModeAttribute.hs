module Rattletrap.Encode.GameModeAttribute
  ( putGameModeAttribute
  ) where

import Rattletrap.Type.GameModeAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

putGameModeAttribute :: GameModeAttribute -> BinaryBit.BitPut ()
putGameModeAttribute gameModeAttribute = do
  let numBits = gameModeAttributeNumBits gameModeAttribute
  let word = gameModeAttributeWord gameModeAttribute
  BinaryBit.putWord8 numBits word
