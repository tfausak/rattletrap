{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.TeamPaintAttribute
  ( TeamPaintAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8
import Rattletrap.Type.Word32

data TeamPaintAttribute = TeamPaintAttribute
  { teamPaintAttributeTeam :: Word8
  , teamPaintAttributePrimaryColor :: Word8
  , teamPaintAttributeAccentColor :: Word8
  , teamPaintAttributePrimaryFinish :: Word32
  , teamPaintAttributeAccentFinish :: Word32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON TeamPaintAttribute where
  parseJSON = defaultParseJson "TeamPaintAttribute"

instance ToJSON TeamPaintAttribute where
  toEncoding = defaultToEncoding "TeamPaintAttribute"
  toJSON = defaultToJson "TeamPaintAttribute"
