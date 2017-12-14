{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.UniqueIdAttribute
  ( UniqueIdAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8
import Rattletrap.Type.RemoteId

data UniqueIdAttribute = UniqueIdAttribute
  { uniqueIdAttributeSystemId :: Word8
  , uniqueIdAttributeRemoteId :: RemoteId
  , uniqueIdAttributeLocalId :: Word8
  } deriving (Eq, Ord, Show)

$(deriveJson ''UniqueIdAttribute)
