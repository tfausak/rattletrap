{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.GameModeAttribute
  ( GameModeAttribute(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Word as Word

data GameModeAttribute = GameModeAttribute
  { gameModeAttributeNumBits :: Int
  , gameModeAttributeWord :: Word.Word8
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON GameModeAttribute where
  parseJSON = defaultParseJson "GameModeAttribute"

instance ToJSON GameModeAttribute where
  toEncoding = defaultToEncoding "GameModeAttribute"
  toJSON = defaultToJson "GameModeAttribute"
