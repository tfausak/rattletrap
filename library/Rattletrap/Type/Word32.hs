{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Word32
  ( Word32(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

newtype Word32 = Word32
  { word32Value :: Word.Word32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Word32 where
  parseJSON = defaultParseJson "Word32"

instance ToJSON Word32 where
  toEncoding = defaultToEncoding "Word32"
  toJSON = defaultToJson "Word32"
