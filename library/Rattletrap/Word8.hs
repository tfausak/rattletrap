module Rattletrap.Word8 where

import qualified Data.Binary as Binary
import qualified Data.Word as Word

newtype Word8 = Word8
  { word8Value :: Word.Word8
  } deriving (Eq, Ord, Show)

getWord8 :: Binary.Get Word8
getWord8 = do
  word8 <- Binary.getWord8
  pure (Word8 word8)

putWord8 :: Word8 -> Binary.Put
putWord8 (Word8 word8) = Binary.putWord8 word8
