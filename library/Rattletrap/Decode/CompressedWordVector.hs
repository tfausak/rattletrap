module Rattletrap.Decode.CompressedWordVector
  ( decodeCompressedWordVectorBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.CompressedWordVector

decodeCompressedWordVectorBits :: (Int, Int, Int) -> DecodeBits CompressedWordVector
decodeCompressedWordVectorBits version =
  CompressedWordVector
    <$> decodeCompressedWordBits (limit version)
    <*> decodeCompressedWordBits (limit version)
    <*> decodeCompressedWordBits (limit version)

limit :: (Int, Int, Int) -> Word
limit version = if version >= (868, 22, 7) then 262144 else 65536
