module Rattletrap.Decode.CompressedWord
  ( getCompressedWord
  ) where

import Rattletrap.Type.CompressedWord

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Bits as Bits

getCompressedWord :: Word -> BinaryBit.BitGet CompressedWord
getCompressedWord limit = do
  value <- getCompressedWordStep limit (getMaxBits limit) 0 0
  pure (CompressedWord limit value)

getMaxBits :: Word -> Word
getMaxBits x = do
  let
    n :: Word
    n = max 1 (ceiling (logBase (2 :: Double) (fromIntegral (max 1 x))))
  if x < 1024 && x == 2 ^ n then n + 1 else n

getCompressedWordStep :: Word -> Word -> Word -> Word -> BinaryBit.BitGet Word
getCompressedWordStep limit maxBits position value = do
  let x = Bits.shiftL 1 (fromIntegral position) :: Word
  if position < maxBits && value + x <= limit
    then do
      bit <- BinaryBit.getBool
      let newValue = if bit then value + x else value
      getCompressedWordStep limit maxBits (position + 1) newValue
    else pure value
