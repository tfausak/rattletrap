module Rattletrap.Encode.Vector
  ( putVector
  ) where

import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Vector

import qualified Data.Binary.Bits.Put as BinaryBits

putVector :: Vector -> BinaryBits.BitPut ()
putVector vector = do
  let
    bitSize =
      round (logBase (2 :: Float) (fromIntegral (vectorBias vector))) - 1 :: Word
    dx =
      fromIntegral (vectorX vector + fromIntegral (vectorBias vector)) :: Word
    dy =
      fromIntegral (vectorY vector + fromIntegral (vectorBias vector)) :: Word
    dz =
      fromIntegral (vectorZ vector + fromIntegral (vectorBias vector)) :: Word
    limit = 2 ^ (bitSize + 2) :: Word
  putCompressedWord (CompressedWord 19 bitSize)
  putCompressedWord (CompressedWord limit dx)
  putCompressedWord (CompressedWord limit dy)
  putCompressedWord (CompressedWord limit dz)
