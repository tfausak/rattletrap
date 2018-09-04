module Rattletrap.Encode.GameModeAttribute
  ( putGameModeAttribute
  )
where

import Rattletrap.Type.GameModeAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putGameModeAttribute :: GameModeAttribute -> BinaryBits.BitPut ()
putGameModeAttribute gameModeAttribute = do
  let numBits = gameModeAttributeNumBits gameModeAttribute
  let word = gameModeAttributeWord gameModeAttribute
  BinaryBits.putWord8 numBits word
