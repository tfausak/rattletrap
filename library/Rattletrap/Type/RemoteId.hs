{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.RemoteId
  ( RemoteId(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word64le

data RemoteId
  = RemoteIdPlayStation Text [Word8]
  | RemoteIdSplitscreen Word32
  | RemoteIdSteam Word64le
  | RemoteIdSwitch [Bool]
  | RemoteIdXbox Word64le
  deriving (Eq, Ord, Show)

$(deriveJson ''RemoteId)
