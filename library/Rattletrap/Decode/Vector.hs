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
  let
    fromCompressedWord :: Num a => CompressedWord -> a
    fromCompressedWord = fromIntegral . compressedWordValue
    bias = 2 ^ (fromCompressedWord bitSize + 1 :: Word)
    x = fromCompressedWord dx - fromIntegral bias
    y = fromCompressedWord dy - fromIntegral bias
    z = fromCompressedWord dz - fromIntegral bias
  pure (Vector bias x y z)
