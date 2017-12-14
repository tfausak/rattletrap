module Rattletrap.Type.CompressedWordVector
  ( CompressedWordVector(..)
  ) where

import Rattletrap.Primitive.CompressedWord

data CompressedWordVector = CompressedWordVector
  { compressedWordVectorX :: CompressedWord
  , compressedWordVectorY :: CompressedWord
  , compressedWordVectorZ :: CompressedWord
  } deriving (Eq, Ord, Show)
