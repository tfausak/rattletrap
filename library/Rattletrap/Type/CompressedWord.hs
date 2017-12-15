{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.CompressedWord
  ( CompressedWord(..)
  ) where

import Rattletrap.Type.Common

data CompressedWord = CompressedWord
  { compressedWordLimit :: Word
  , compressedWordValue :: Word
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON CompressedWord where
  parseJSON = defaultParseJson "CompressedWord"

instance ToJSON CompressedWord where
  toEncoding = defaultToEncoding "CompressedWord"
  toJSON = defaultToJson "CompressedWord"
