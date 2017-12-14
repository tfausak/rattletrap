module Rattletrap.Type.AttributeMapping
  ( AttributeMapping(..)
  ) where

import Rattletrap.Type.Word32

data AttributeMapping = AttributeMapping
  { attributeMappingObjectId :: Word32
  , attributeMappingStreamId :: Word32
  } deriving (Eq, Ord, Show)
