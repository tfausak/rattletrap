module Rattletrap.Type.Vector where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

data Vector = Vector
  { size :: CompressedWord.CompressedWord
  , bias :: Word
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Word' rather than something more precise like a
  -- 'Word8' because it just gets passed to a functions that expect 'Word's.
  -- There's no reason to do a bunch of conversions.
  , x :: Int
  -- ^ See 'bias'.
  , y :: Int
  -- ^ See 'bias'.
  , z :: Int
  -- ^ See 'bias'.
  }
  deriving (Eq, Show)

$(deriveJson ''Vector)

schema :: Schema.Schema
schema = Schema.named "vector" $ Schema.object
  [ (Json.pair "size" $ Schema.ref CompressedWord.schema, True)
  , (Json.pair "bias" $ Schema.json Schema.integer, True)
  , (Json.pair "x" $ Schema.json Schema.integer, True)
  , (Json.pair "y" $ Schema.json Schema.integer, True)
  , (Json.pair "z" $ Schema.json Schema.integer, True)
  ]

bitPut :: Vector -> BitPut.BitPut
bitPut vector =
  let
    bitSize =
      round (logBase (2 :: Float) (fromIntegral (bias vector))) - 1 :: Word
    dx = fromIntegral (x vector + fromIntegral (bias vector)) :: Word
    dy = fromIntegral (y vector + fromIntegral (bias vector)) :: Word
    dz = fromIntegral (z vector + fromIntegral (bias vector)) :: Word
    limit = 2 ^ (bitSize + 2) :: Word
  in
    CompressedWord.bitPut (size vector)
    <> CompressedWord.bitPut (CompressedWord.CompressedWord limit dx)
    <> CompressedWord.bitPut (CompressedWord.CompressedWord limit dy)
    <> CompressedWord.bitPut (CompressedWord.CompressedWord limit dz)

bitGet :: Version.Version -> BitGet.BitGet Vector
bitGet version = do
  size_ <- CompressedWord.bitGet (if has21Bits version then 21 else 19)
  let
    limit = getLimit size_
    bias_ = getBias size_
  Vector size_ bias_
    <$> fmap (fromDelta bias_) (CompressedWord.bitGet limit)
    <*> fmap (fromDelta bias_) (CompressedWord.bitGet limit)
    <*> fmap (fromDelta bias_) (CompressedWord.bitGet limit)

has21Bits :: Version.Version -> Bool
has21Bits v =
  Version.major v >= 868 && Version.minor v >= 22 && Version.patch v >= 7

getLimit :: CompressedWord.CompressedWord -> Word
getLimit = (2 ^) . (+ 2) . CompressedWord.value

getBias :: CompressedWord.CompressedWord -> Word
getBias = (2 ^) . (+ 1) . CompressedWord.value

fromDelta :: Word -> CompressedWord.CompressedWord -> Int
fromDelta bias_ x_ =
  fromIntegral (CompressedWord.value x_) - fromIntegral bias_
