{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.TeamPaintAttribute
  ( TeamPaintAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le
import Rattletrap.Type.Word32

data TeamPaintAttribute = TeamPaintAttribute
  { teamPaintAttributeTeam :: Word8le
  , teamPaintAttributePrimaryColor :: Word8le
  , teamPaintAttributeAccentColor :: Word8le
  , teamPaintAttributePrimaryFinish :: Word32
  , teamPaintAttributeAccentFinish :: Word32
  } deriving (Eq, Ord, Show)

$(deriveJson ''TeamPaintAttribute)
