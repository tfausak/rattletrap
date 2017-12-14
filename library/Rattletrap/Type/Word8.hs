{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Word8
  ( Word8(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

newtype Word8 = Word8
  { word8Value :: Word.Word8
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Word8 where
  parseJSON = defaultParseJson "word8"

instance ToJSON Word8 where
  toEncoding = defaultToEncoding "word8"
  toJSON = defaultToJson "word8"
