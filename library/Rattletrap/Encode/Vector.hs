module Rattletrap.Encode.Vector
  ( putVector
  ) where

import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Vector

import qualified Data.Binary.Bits.Put as BinaryBit

putVector :: Vector -> BinaryBit.BitPut ()
putVector vector = do
  let
    bitSize =
      round (logBase (2 :: Float) (fromIntegral (vectorBias vector))) - 1
  putCompressedWord (CompressedWord 19 bitSize)
  let dx = fromIntegral (vectorX vector + fromIntegral (vectorBias vector))
  let dy = fromIntegral (vectorY vector + fromIntegral (vectorBias vector))
  let dz = fromIntegral (vectorZ vector + fromIntegral (vectorBias vector))
  let limit = 2 ^ (bitSize + 2)
  putCompressedWord (CompressedWord limit dx)
  putCompressedWord (CompressedWord limit dy)
  putCompressedWord (CompressedWord limit dz)
