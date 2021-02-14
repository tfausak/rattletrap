{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CompressedWord where

import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.Bits as Bits

-- | Although there's no guarantee that these values will not overflow, it's
-- exceptionally unlikely. Most 'CompressedWord's are very small.
data CompressedWord = CompressedWord
  { limit :: Word
  , value :: Word
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''CompressedWord)

bitPut :: CompressedWord -> BitPut ()
bitPut compressedWord =
  let
    limit_ = limit compressedWord
    value_ = value compressedWord
    maxBits = getMaxBits limit_
  in putCompressedWordStep limit_ value_ maxBits 0 0

putCompressedWordStep
  :: Word -> Word -> Int -> Int -> Word -> BitPut ()
putCompressedWordStep limit_ value_ maxBits position soFar =
  if position < maxBits
    then do
      let x = Bits.shiftL 1 position :: Word
      if maxBits > 1 && position == maxBits - 1 && soFar + x > limit_
        then pure ()
        else do
          let bit = Bits.testBit value_ position
          BinaryBits.putBool bit
          let delta = if bit then x else 0
          putCompressedWordStep
            limit_
            value_
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

bitGet :: Word -> BitGet CompressedWord
bitGet limit_ =
  CompressedWord limit_ <$> step limit_ (getMaxBits_ limit_) 0 0

getMaxBits_ :: Word -> Word
getMaxBits_ x = do
  let
    n :: Word
    n = max 1 (ceiling (logBase (2 :: Double) (fromIntegral (max 1 x))))
  if x < 1024 && x == 2 ^ n then n + 1 else n

step :: Word -> Word -> Word -> Word -> BitGet Word
step limit_ maxBits position value_ = do
  let x = Bits.shiftL 1 (fromIntegral position) :: Word
  if position < maxBits && value_ + x <= limit_
    then do
      bit <- getBool
      let newValue = if bit then value_ + x else value_
      step limit_ maxBits (position + 1) newValue
    else pure value_
