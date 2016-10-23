module Rattletrap.AttributeValue.GameMode where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

data GameModeAttributeValue = GameModeAttributeValue
  { gameModeAttributeValueNumBits :: Int
  , gameModeAttributeValueWord :: Word.Word8
  } deriving (Eq, Ord, Show)

getGameModeAttributeValue :: (Int, Int)
                          -> BinaryBit.BitGet GameModeAttributeValue
getGameModeAttributeValue version = do
  let numBits =
        if version < (868, 12)
          then 2
          else 8
  word <- BinaryBit.getWord8 numBits
  pure (GameModeAttributeValue numBits word)

putGameModeAttributeValue :: GameModeAttributeValue -> BinaryBit.BitPut ()
putGameModeAttributeValue gameModeAttributeValue = do
  let numBits = gameModeAttributeValueNumBits gameModeAttributeValue
  let word = gameModeAttributeValueWord gameModeAttributeValue
  BinaryBit.putWord8 numBits word
