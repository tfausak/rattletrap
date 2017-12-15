module Rattletrap.Encode.CompressedWord
  ( putCompressedWord
  ) where

import Rattletrap.Type.CompressedWord

import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Bits as Bits

putCompressedWord :: CompressedWord -> BinaryBit.BitPut ()
putCompressedWord compressedWord =
  let
    limit = compressedWordLimit compressedWord
    value = compressedWordValue compressedWord
    maxBits = getMaxBits limit
  in putCompressedWordStep limit value maxBits 0 0

putCompressedWordStep
  :: Word -> Word -> Int -> Int -> Word -> BinaryBit.BitPut ()
putCompressedWordStep limit value maxBits position soFar =
  if position < maxBits
    then do
      let x = Bits.shiftL 1 position :: Word
      if maxBits > 1 && position == maxBits - 1 && soFar + x > limit
        then pure ()
        else do
          let bit = Bits.testBit value position
          BinaryBit.putBool bit
          let delta = if bit then x else 0
          putCompressedWordStep
            limit
            value
            maxBits
            (position + 1)
            (soFar + delta)
    else pure ()

getMaxBits :: Word -> Int
getMaxBits x =
  let
    n :: Int
    n = max 1 (ceiling (logBase (2 :: Double) (fromIntegral (max 1 x))))
  in if x < 1024 && x == 2 ^ n then n + 1 else n
