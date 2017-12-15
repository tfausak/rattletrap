{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.UniqueIdAttribute
  ( UniqueIdAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le
import Rattletrap.Type.RemoteId

data UniqueIdAttribute = UniqueIdAttribute
  { uniqueIdAttributeSystemId :: Word8le
  , uniqueIdAttributeRemoteId :: RemoteId
  , uniqueIdAttributeLocalId :: Word8le
  } deriving (Eq, Ord, Show)

$(deriveJson ''UniqueIdAttribute)
