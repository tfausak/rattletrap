module Rattletrap.Decode.CompressedWordVector
  ( getCompressedWordVector
  ) where

import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.CompressedWordVector

import qualified Data.Binary.Bits.Get as BinaryBit

getCompressedWordVector :: BinaryBit.BitGet CompressedWordVector
getCompressedWordVector = do
  let limit = 65536 :: Word
  x <- getCompressedWord limit
  y <- getCompressedWord limit
  z <- getCompressedWord limit
  pure (CompressedWordVector x y z)
