module Rattletrap.Primitive.CompressedWord where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Bits as Bits

data CompressedWord = CompressedWord
  { compressedWordLimit :: Word
  , compressedWordValue :: Word
  } deriving (Eq, Ord, Show)

getCompressedWord :: Word -> BinaryBit.BitGet CompressedWord
getCompressedWord limit = do
  value <- getCompressedWordStep limit (getMaxBits limit) 0 0
  pure (CompressedWord limit value)

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

getCompressedWordStep :: Word -> Word -> Word -> Word -> BinaryBit.BitGet Word
getCompressedWordStep limit maxBits position value = do
  let x = Bits.shiftL 1 (fromIntegral position)
  if position < maxBits && value + x <= limit
    then do
      bit <- BinaryBit.getBool
      let newValue = if bit then value + x else value
      getCompressedWordStep limit maxBits (position + 1) newValue
    else pure value
