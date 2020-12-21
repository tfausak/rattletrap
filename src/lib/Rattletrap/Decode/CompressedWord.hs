module Rattletrap.Decode.CompressedWord
  ( decodeCompressedWordBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.CompressedWord

import qualified Data.Bits as Bits

decodeCompressedWordBits :: Word -> DecodeBits CompressedWord
decodeCompressedWordBits limit =
  CompressedWord limit <$> step limit (getMaxBits limit) 0 0

getMaxBits :: Word -> Word
getMaxBits x = do
  let
    n :: Word
    n = max 1 (ceiling (logBase (2 :: Double) (fromIntegral (max 1 x))))
  if x < 1024 && x == 2 ^ n then n + 1 else n

step :: Word -> Word -> Word -> Word -> DecodeBits Word
step limit maxBits position value = do
  let x = Bits.shiftL 1 (fromIntegral position) :: Word
  if position < maxBits && value + x <= limit
    then do
      bit <- getBool
      let newValue = if bit then value + x else value
      step limit maxBits (position + 1) newValue
    else pure value
