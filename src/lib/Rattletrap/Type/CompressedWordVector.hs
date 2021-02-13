{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CompressedWordVector where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data CompressedWordVector = CompressedWordVector
  { compressedWordVectorX :: CompressedWord
  , compressedWordVectorY :: CompressedWord
  , compressedWordVectorZ :: CompressedWord
  }
  deriving (Eq, Show)

$(deriveJson ''CompressedWordVector)

putCompressedWordVector :: CompressedWordVector -> BitPut ()
putCompressedWordVector compressedWordVector = do
  putCompressedWord (compressedWordVectorX compressedWordVector)
  putCompressedWord (compressedWordVectorY compressedWordVector)
  putCompressedWord (compressedWordVectorZ compressedWordVector)

decodeCompressedWordVectorBits :: BitGet CompressedWordVector
decodeCompressedWordVectorBits =
  CompressedWordVector
    <$> decodeCompressedWordBits limit
    <*> decodeCompressedWordBits limit
    <*> decodeCompressedWordBits limit

limit :: Word
limit = 65536
