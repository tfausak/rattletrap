{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Vector where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Vector = Vector
  { size :: CompressedWord
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

$(deriveJsonWith ''Vector jsonOptions)

bitPut :: Vector -> BitPut ()
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
  putCompressedWord (size vector)
  putCompressedWord (CompressedWord limit dx)
  putCompressedWord (CompressedWord limit dy)
  putCompressedWord (CompressedWord limit dz)

bitGet :: (Int, Int, Int) -> BitGet Vector
bitGet version = do
  size_ <- decodeCompressedWordBits (if version >= (868, 22, 7) then 21 else 19)
  let
    limit = getLimit size_
    bias_ = getBias size_
  Vector size_ bias_
    <$> fmap (fromDelta bias_) (decodeCompressedWordBits limit)
    <*> fmap (fromDelta bias_) (decodeCompressedWordBits limit)
    <*> fmap (fromDelta bias_) (decodeCompressedWordBits limit)

getLimit :: CompressedWord -> Word
getLimit = (2 ^) . (+ 2) . compressedWordValue

getBias :: CompressedWord -> Word
getBias = (2 ^) . (+ 1) . compressedWordValue

fromDelta :: Word -> CompressedWord -> Int
fromDelta bias_ x_ = fromIntegral (compressedWordValue x_) - fromIntegral bias_
