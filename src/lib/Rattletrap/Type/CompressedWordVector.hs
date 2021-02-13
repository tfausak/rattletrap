{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CompressedWordVector where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data CompressedWordVector = CompressedWordVector
  { x :: CompressedWord.CompressedWord
  , y :: CompressedWord.CompressedWord
  , z :: CompressedWord.CompressedWord
  }
  deriving (Eq, Show)

$(deriveJsonWith ''CompressedWordVector jsonOptions)

bitPut :: CompressedWordVector -> BitPut ()
bitPut compressedWordVector = do
  CompressedWord.bitPut (x compressedWordVector)
  CompressedWord.bitPut (y compressedWordVector)
  CompressedWord.bitPut (z compressedWordVector)

bitGet :: BitGet CompressedWordVector
bitGet =
  CompressedWordVector
    <$> CompressedWord.bitGet limit
    <*> CompressedWord.bitGet limit
    <*> CompressedWord.bitGet limit

limit :: Word
limit = 65536
