module Rattletrap.Decode.Vector
  ( getVector
  ) where

import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Vector

import qualified Data.Binary.Bits.Get as BinaryBit

getVector :: BinaryBit.BitGet Vector
getVector = do
  bitSize <- getCompressedWord 19
  let limit = 2 ^ (compressedWordValue bitSize + 2)
  dx <- getCompressedWord limit
  dy <- getCompressedWord limit
  dz <- getCompressedWord limit
  let fromCompressedWord x = fromIntegral (compressedWordValue x)
  let bias = 2 ^ (fromCompressedWord bitSize + 1 :: Word)
  let x = fromCompressedWord dx - fromIntegral bias
  let y = fromCompressedWord dy - fromIntegral bias
  let z = fromCompressedWord dz - fromIntegral bias
  pure (Vector bias x y z)
