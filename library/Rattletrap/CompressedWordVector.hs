module Rattletrap.CompressedWordVector where

import Rattletrap.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data CompressedWordVector = CompressedWordVector
  { compressedWordVectorX :: CompressedWord
  , compressedWordVectorY :: CompressedWord
  , compressedWordVectorZ :: CompressedWord
  } deriving (Eq, Ord, Show)

getCompressedWordVector :: BinaryBit.BitGet CompressedWordVector
getCompressedWordVector = do
  let limit = 65536
  x <- getCompressedWord limit
  y <- getCompressedWord limit
  z <- getCompressedWord limit
  pure
    CompressedWordVector
    { compressedWordVectorX = x
    , compressedWordVectorY = y
    , compressedWordVectorZ = z
    }

putCompressedWordVector :: CompressedWordVector -> BinaryBit.BitPut ()
putCompressedWordVector compressedWordVector = do
  putCompressedWord (compressedWordVectorX compressedWordVector)
  putCompressedWord (compressedWordVectorY compressedWordVector)
  putCompressedWord (compressedWordVectorZ compressedWordVector)
