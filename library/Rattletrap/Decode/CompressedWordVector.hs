module Rattletrap.Decode.CompressedWordVector
  ( decodeCompressedWordVectorBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.CompressedWordVector

decodeCompressedWordVectorBits :: DecodeBits CompressedWordVector
decodeCompressedWordVectorBits =
  CompressedWordVector
    <$> decodeCompressedWordBits limit
    <*> decodeCompressedWordBits limit
    <*> decodeCompressedWordBits limit

limit :: Word
limit = 65536
