{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Cache
  ( Cache(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.AttributeMapping
import Rattletrap.Type.Word32
import Rattletrap.Type.List

data Cache = Cache
  { cacheClassId :: Word32
  , cacheParentCacheId :: Word32
  , cacheCacheId :: Word32
  , cacheAttributeMappings :: List AttributeMapping
  } deriving (Eq, Ord, Show)

$(deriveJson ''Cache)
