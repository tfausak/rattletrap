{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.MusicStingerAttribute
  ( MusicStingerAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32
import Rattletrap.Type.Word8le

data MusicStingerAttribute = MusicStingerAttribute
  { musicStingerAttributeFlag :: Bool
  , musicStingerAttributeCue :: Word32
  , musicStingerAttributeTrigger :: Word8le
  } deriving (Eq, Ord, Show)

$(deriveJson ''MusicStingerAttribute)
