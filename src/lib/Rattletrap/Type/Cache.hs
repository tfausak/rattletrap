{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Cache
  ( Cache(..)
  ) where

import Rattletrap.Type.AttributeMapping
import Rattletrap.Type.Common
import Rattletrap.Type.List
import Rattletrap.Type.Word32le

data Cache = Cache
  { cacheClassId :: Word32le
  , cacheParentCacheId :: Word32le
  , cacheCacheId :: Word32le
  , cacheAttributeMappings :: List AttributeMapping
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Cache)
