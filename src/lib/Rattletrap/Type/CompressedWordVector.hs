{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CompressedWordVector where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord

import qualified Data.Binary.Bits.Put as BinaryBits

data CompressedWordVector = CompressedWordVector
  { compressedWordVectorX :: CompressedWord
  , compressedWordVectorY :: CompressedWord
  , compressedWordVectorZ :: CompressedWord
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''CompressedWordVector)

putCompressedWordVector :: CompressedWordVector -> BinaryBits.BitPut ()
putCompressedWordVector compressedWordVector = do
  putCompressedWord (compressedWordVectorX compressedWordVector)
  putCompressedWord (compressedWordVectorY compressedWordVector)
  putCompressedWord (compressedWordVectorZ compressedWordVector)
