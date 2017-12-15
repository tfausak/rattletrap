{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.RemoteId
  ( RemoteId(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word64

import qualified Data.Text as Text
import qualified Data.Word as Word

data RemoteId
  = RemoteIdPlayStation Text.Text [Word.Word8]
  | RemoteIdSplitscreen Word.Word32
  | RemoteIdSteam Word64
  | RemoteIdXbox Word64
  deriving (Eq, Generic, Ord, Show)

instance FromJSON RemoteId where
  parseJSON = defaultParseJson "RemoteId"

instance ToJSON RemoteId where
  toEncoding = defaultToEncoding "RemoteId"
  toJSON = defaultToJson "RemoteId"
