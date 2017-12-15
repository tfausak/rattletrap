{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CompressedWord
  ( CompressedWord(..)
  ) where

import Rattletrap.Type.Common

data CompressedWord = CompressedWord
  { compressedWordLimit :: Word
  , compressedWordValue :: Word
  } deriving (Eq, Ord, Show)

$(deriveJson ''CompressedWord)
