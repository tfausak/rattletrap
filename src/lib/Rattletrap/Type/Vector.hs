{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Vector where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

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

bitPut :: Vector -> BitPut.BitPut
bitPut vector = do
  let
    bitSize =
      round (logBase (2 :: Float) (fromIntegral (bias vector))) - 1 :: Word
    dx =
      fromIntegral (x vector + fromIntegral (bias vector)) :: Word
    dy =
      fromIntegral (y vector + fromIntegral (bias vector)) :: Word
    dz =
      fromIntegral (z vector + fromIntegral (bias vector)) :: Word
    limit = 2 ^ (bitSize + 2) :: Word
  CompressedWord.bitPut (size vector)
  CompressedWord.bitPut (CompressedWord.CompressedWord limit dx)
  CompressedWord.bitPut (CompressedWord.CompressedWord limit dy)
  CompressedWord.bitPut (CompressedWord.CompressedWord limit dz)

bitGet :: (Int, Int, Int) -> BitGet.BitGet Vector
bitGet version = do
  size_ <- CompressedWord.bitGet (if version >= (868, 22, 7) then 21 else 19)
  let
    limit = getLimit size_
    bias_ = getBias size_
  Vector size_ bias_
    <$> fmap (fromDelta bias_) (CompressedWord.bitGet limit)
    <*> fmap (fromDelta bias_) (CompressedWord.bitGet limit)
    <*> fmap (fromDelta bias_) (CompressedWord.bitGet limit)

getLimit :: CompressedWord.CompressedWord -> Word
getLimit = (2 ^) . (+ 2) . CompressedWord.value

getBias :: CompressedWord.CompressedWord -> Word
getBias = (2 ^) . (+ 1) . CompressedWord.value

fromDelta :: Word -> CompressedWord.CompressedWord -> Int
fromDelta bias_ x_ = fromIntegral (CompressedWord.value x_) - fromIntegral bias_
