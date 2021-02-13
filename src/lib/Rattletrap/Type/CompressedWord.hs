{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CompressedWord where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Bits as Bits

-- | Although there's no guarantee that these values will not overflow, it's
-- exceptionally unlikely. Most 'CompressedWord's are very small.
data CompressedWord = CompressedWord
  { compressedWordLimit :: Word
  , compressedWordValue :: Word
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''CompressedWord)

putCompressedWord :: CompressedWord -> BinaryBits.BitPut ()
putCompressedWord compressedWord =
  let
    limit = compressedWordLimit compressedWord
    value = compressedWordValue compressedWord
    maxBits = getMaxBits limit
  in putCompressedWordStep limit value maxBits 0 0

putCompressedWordStep
  :: Word -> Word -> Int -> Int -> Word -> BinaryBits.BitPut ()
putCompressedWordStep limit value maxBits position soFar =
  if position < maxBits
    then do
      let x = Bits.shiftL 1 position :: Word
      if maxBits > 1 && position == maxBits - 1 && soFar + x > limit
        then pure ()
        else do
          let bit = Bits.testBit value position
          BinaryBits.putBool bit
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

decodeCompressedWordBits :: Word -> DecodeBits CompressedWord
decodeCompressedWordBits limit =
  CompressedWord limit <$> step limit (getMaxBits_ limit) 0 0

getMaxBits_ :: Word -> Word
getMaxBits_ x = do
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
