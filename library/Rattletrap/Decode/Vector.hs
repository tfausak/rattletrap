module Rattletrap.Decode.Vector
  ( decodeVectorBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Vector

decodeVectorBits :: DecodeBits Vector
decodeVectorBits = do
  size <- decodeCompressedWordBits 19
  let
    limit = getLimit size
    bias = getBias size
  Vector bias
    <$> fmap (fromDelta bias) (decodeCompressedWordBits limit)
    <*> fmap (fromDelta bias) (decodeCompressedWordBits limit)
    <*> fmap (fromDelta bias) (decodeCompressedWordBits limit)

getLimit :: CompressedWord -> Word
getLimit = (2 ^) . (+ 2) . compressedWordValue

getBias :: CompressedWord -> Word
getBias = (2 ^) . (+ 1) . compressedWordValue

fromDelta :: Word -> CompressedWord -> Int
fromDelta bias x = fromIntegral (compressedWordValue x) - fromIntegral bias
