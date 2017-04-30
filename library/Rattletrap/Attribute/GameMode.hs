module Rattletrap.Attribute.GameMode where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

data GameModeAttribute = GameModeAttribute
  { gameModeAttributeNumBits :: Int
  , gameModeAttributeWord :: Word.Word8
  } deriving (Eq, Ord, Show)

getGameModeAttribute :: (Int, Int) -> BinaryBit.BitGet GameModeAttribute
getGameModeAttribute version = do
  let numBits =
        if version < (868, 12)
          then 2
          else 8
  word <- BinaryBit.getWord8 numBits
  pure (GameModeAttribute numBits word)

putGameModeAttribute :: GameModeAttribute -> BinaryBit.BitPut ()
putGameModeAttribute gameModeAttribute = do
  let numBits = gameModeAttributeNumBits gameModeAttribute
  let word = gameModeAttributeWord gameModeAttribute
  BinaryBit.putWord8 numBits word
