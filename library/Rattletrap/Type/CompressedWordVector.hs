module Rattletrap.Type.CompressedWordVector
  ( CompressedWordVector(..)
  ) where

import Rattletrap.Type.CompressedWord

data CompressedWordVector = CompressedWordVector
  { compressedWordVectorX :: CompressedWord
  , compressedWordVectorY :: CompressedWord
  , compressedWordVectorZ :: CompressedWord
  } deriving (Eq, Ord, Show)
