module Rattletrap.Primitive.Vector where

import Rattletrap.Primitive.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data Vector = Vector
  { vectorBitSize :: CompressedWord
  , vectorDx :: CompressedWord
  , vectorDy :: CompressedWord
  , vectorDz :: CompressedWord
  } deriving (Eq, Ord, Show)

getVector :: BinaryBit.BitGet Vector
getVector = do
  bitSize <- getCompressedWord 19
  let limit = 2 ^ (compressedWordValue bitSize + 2)
  dx <- getCompressedWord limit
  dy <- getCompressedWord limit
  dz <- getCompressedWord limit
  pure (Vector bitSize dx dy dz)

putVector :: Vector -> BinaryBit.BitPut ()
putVector vector = do
  putCompressedWord (vectorBitSize vector)
  putCompressedWord (vectorDx vector)
  putCompressedWord (vectorDy vector)
  putCompressedWord (vectorDz vector)
