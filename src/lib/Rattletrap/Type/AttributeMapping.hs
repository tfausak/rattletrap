{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeMapping
  ( AttributeMapping(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le

data AttributeMapping = AttributeMapping
  { attributeMappingObjectId :: Word32le
  , attributeMappingStreamId :: Word32le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''AttributeMapping)
