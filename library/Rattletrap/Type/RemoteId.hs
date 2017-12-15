{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.RemoteId
  ( RemoteId(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word64le

import qualified Data.Text as Text
import qualified Data.Word as Word

data RemoteId
  = RemoteIdPlayStation Text.Text [Word.Word8]
  | RemoteIdSplitscreen Word.Word32
  | RemoteIdSteam Word64le
  | RemoteIdXbox Word64le
  deriving (Eq, Ord, Show)

$(deriveJson ''RemoteId)
