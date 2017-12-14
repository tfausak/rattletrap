module Rattletrap.Encode.CompressedWord
  ( putCompressedWord
  ) where

import Rattletrap.Type.CompressedWord

import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Bits as Bits

putCompressedWord :: CompressedWord -> BinaryBit.BitPut ()
putCompressedWord compressedWord = do
  let limit = compressedWordLimit compressedWord
  let value = compressedWordValue compressedWord
  let maxBits = getMaxBits limit
  let
    go position soFar = if position < maxBits
      then do
        let x = Bits.shiftL 1 position
        if maxBits > 1 && position == maxBits - 1 && soFar + x > limit
          then pure ()
          else do
            let bit = Bits.testBit value position
            BinaryBit.putBool bit
            let delta = if bit then x else 0
            go (position + 1) (soFar + delta)
      else pure ()
  go 0 0

getMaxBits :: (Integral a, Integral b) => a -> b
getMaxBits x = do
  let n = max 1 (ceiling (logBase (2 :: Double) (fromIntegral (max 1 x))))
  if x < 1024 && x == 2 ^ n then n + 1 else n
