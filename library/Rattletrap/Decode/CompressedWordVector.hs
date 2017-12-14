module Rattletrap.Decode.CompressedWordVector
  ( getCompressedWordVector
  ) where

import Rattletrap.Primitive.CompressedWord
import Rattletrap.Type.CompressedWordVector

import qualified Data.Binary.Bits.Get as BinaryBit

getCompressedWordVector :: BinaryBit.BitGet CompressedWordVector
getCompressedWordVector = do
  let limit = 65536
  x <- getCompressedWord limit
  y <- getCompressedWord limit
  z <- getCompressedWord limit
  pure (CompressedWordVector x y z)
