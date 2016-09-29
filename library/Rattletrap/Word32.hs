module Rattletrap.Word32 where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Word as Word

newtype Word32 = Word32
  { word32Value :: Word.Word32
  } deriving (Eq, Ord, Show)

getWord32 :: Binary.Get Word32
getWord32 = do
  word32 <- Binary.getWord32le
  pure (Word32 word32)

putWord32 :: Word32 -> Binary.Put
putWord32 (Word32 word32) = Binary.putWord32le word32
