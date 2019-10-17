{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CompressedWord
  ( CompressedWord(..)
  )
where

import Rattletrap.Type.Common

-- | Although there's no guarantee that these values will not overflow, it's
-- exceptionally unlikely. Most 'CompressedWord's are very small.
data CompressedWord = CompressedWord
  { compressedWordLimit :: Word
  , compressedWordValue :: Word
  } deriving (Eq, Ord, Show)

$(deriveJson ''CompressedWord)
