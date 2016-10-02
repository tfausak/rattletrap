module Rattletrap.Location where

import Rattletrap.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Location = Location
  { locationBitSize :: CompressedWord
  , locationDx :: CompressedWord
  , locationDy :: CompressedWord
  , locationDz :: CompressedWord
  } deriving (Eq, Ord, Show)

getLocation :: BinaryBit.BitGet Location
getLocation = do
  bitSize <- getCompressedWord 19
  let limit = 2 ^ (compressedWordValue bitSize + 2)
  dx <- getCompressedWord limit
  dy <- getCompressedWord limit
  dz <- getCompressedWord limit
  pure
    Location
    { locationBitSize = bitSize
    , locationDx = dx
    , locationDy = dy
    , locationDz = dz
    }

putLocation :: Location -> BinaryBit.BitPut ()
putLocation location = do
  putCompressedWord (locationBitSize location)
  putCompressedWord (locationDx location)
  putCompressedWord (locationDy location)
  putCompressedWord (locationDz location)
