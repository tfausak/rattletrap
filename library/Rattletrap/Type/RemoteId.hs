{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.RemoteId
  ( RemoteId(..)
  )
where

import Rattletrap.Type.Bitstream
import Rattletrap.Type.Common
import Rattletrap.Type.Word64le

data RemoteId
  = RemoteIdPlayStation Text [Word8]
  | RemoteIdPsyNet Bitstream
  | RemoteIdSplitscreen Word32
  -- ^ Really only 24 bits.
  | RemoteIdSteam Word64le
  | RemoteIdSwitch Word64le Word64le Word64le Word64le
  | RemoteIdXbox Word64le
  deriving (Eq, Ord, Show)

$(deriveJson ''RemoteId)
