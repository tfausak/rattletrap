{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.CompressedWordVector
  ( CompressedWordVector(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord

data CompressedWordVector = CompressedWordVector
  { compressedWordVectorX :: CompressedWord
  , compressedWordVectorY :: CompressedWord
  , compressedWordVectorZ :: CompressedWord
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON CompressedWordVector where
  parseJSON = defaultParseJson "CompressedWordVector"

instance ToJSON CompressedWordVector where
  toEncoding = defaultToEncoding "CompressedWordVector"
  toJSON = defaultToJson "CompressedWordVector"
