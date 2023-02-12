module Rattletrap.Type.CompressedWord where

import qualified Data.Bits as Bits
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

-- | Although there's no guarantee that these values will not overflow, it's
-- exceptionally unlikely. Most 'CompressedWord's are very small.
data CompressedWord = CompressedWord
  { limit :: Word,
    value :: Word
  }
  deriving (Eq, Ord, Show)

instance Json.FromJSON CompressedWord where
  parseJSON = Json.withObject "CompressedWord" $ \object -> do
    limit <- Json.required object "limit"
    value <- Json.required object "value"
    pure CompressedWord {limit, value}

instance Json.ToJSON CompressedWord where
  toJSON x =
    Json.object [Json.pair "limit" $ limit x, Json.pair "value" $ value x]

schema :: Schema.Schema
schema =
  Schema.named "compressedWord" $
    Schema.object
      [ (Json.pair "limit" $ Json.object [Json.pair "type" "integer"], True),
        (Json.pair "value" $ Json.object [Json.pair "type" "integer"], True)
      ]

bitPut :: CompressedWord -> BitPut.BitPut
bitPut compressedWord =
  let limit_ = limit compressedWord
      value_ = value compressedWord
      maxBits = getMaxBits limit_
   in putCompressedWordStep limit_ value_ maxBits 0 0

putCompressedWordStep :: Word -> Word -> Int -> Int -> Word -> BitPut.BitPut
putCompressedWordStep limit_ value_ maxBits position soFar =
  if position < maxBits
    then do
      let x = Bits.shiftL 1 position :: Word
      if maxBits > 1 && position == maxBits - 1 && soFar + x > limit_
        then mempty
        else
          let bit = Bits.testBit value_ position
              delta = if bit then x else 0
           in BitPut.bool bit
                <> putCompressedWordStep
                  limit_
                  value_
                  maxBits
                  (position + 1)
                  (soFar + delta)
    else mempty

getMaxBits :: Word -> Int
getMaxBits x =
  let n :: Int
      n = max 1 (ceiling (logBase (2 :: Double) (fromIntegral (max 1 x))))
   in if x < 1024 && x == 2 ^ n then n + 1 else n

bitGet :: Word -> BitGet.BitGet CompressedWord
bitGet = bitGetNew

bitGetNew :: Word -> BitGet.BitGet CompressedWord
bitGetNew limit = do
  value <-
    if limit < 1
      then pure 0
      else do
        let numBits =
              max (0 :: Int)
                . subtract 1
                . ceiling
                . logBase (2 :: Double)
                $ fromIntegral limit
        partial <- BitGet.bits numBits
        let next = partial + Bits.shiftL 1 numBits
        if next > limit
          then pure partial
          else do
            x <- BitGet.bool
            pure $ if x then next else partial
  pure CompressedWord {limit, value}
