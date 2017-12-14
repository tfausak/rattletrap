module Rattletrap.Type.CompressedWord
  ( CompressedWord(..)
  ) where

data CompressedWord = CompressedWord
  { compressedWordLimit :: Word
  , compressedWordValue :: Word
  } deriving (Eq, Ord, Show)
