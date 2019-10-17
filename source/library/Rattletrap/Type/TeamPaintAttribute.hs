{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.TeamPaintAttribute
  ( TeamPaintAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le

data TeamPaintAttribute = TeamPaintAttribute
  { teamPaintAttributeTeam :: Word8le
  , teamPaintAttributePrimaryColor :: Word8le
  , teamPaintAttributeAccentColor :: Word8le
  , teamPaintAttributePrimaryFinish :: Word32le
  , teamPaintAttributeAccentFinish :: Word32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''TeamPaintAttribute)
