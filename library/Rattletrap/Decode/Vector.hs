module Rattletrap.Decode.Vector
  ( getVector
  ) where

import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getVector :: BinaryBit.BitGet Vector
getVector = do
  bitSize <- decodeCompressedWordBits 19
  let limit = 2 ^ (compressedWordValue bitSize + 2) :: Word
  dx <- decodeCompressedWordBits limit
  dy <- decodeCompressedWordBits limit
  dz <- decodeCompressedWordBits limit
  let
    fromCompressedWord :: Num a => CompressedWord -> a
    fromCompressedWord = fromIntegral . compressedWordValue
    bias = 2 ^ (fromCompressedWord bitSize + 1 :: Word) :: Word
    x = fromCompressedWord dx - fromIntegral bias :: Int
    y = fromCompressedWord dy - fromIntegral bias :: Int
    z = fromCompressedWord dz - fromIntegral bias :: Int
  pure (Vector bias x y z)
