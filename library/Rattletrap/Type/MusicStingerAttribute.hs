module Rattletrap.Type.MusicStingerAttribute
  ( MusicStingerAttribute(..)
  ) where

import Rattletrap.Type.Word32
import Rattletrap.Type.Word8

data MusicStingerAttribute = MusicStingerAttribute
  { musicStingerAttributeFlag :: Bool
  , musicStingerAttributeCue :: Word32
  , musicStingerAttributeTrigger :: Word8
  } deriving (Eq, Ord, Show)
