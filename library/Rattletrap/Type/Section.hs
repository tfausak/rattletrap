{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Section
  ( Section(..)
  ) where

import Rattletrap.Type.Common

-- | A section is a large piece of a 'Rattletrap.Replay.Replay'. It has a
-- 32-bit size (in bytes), a 32-bit CRC (see "Rattletrap.Utility.Crc"), and then a
-- bunch of data (the body). This interface is provided so that you don't have
-- to think about the size and CRC.
newtype Section a = Section
  { sectionBody :: a
  -- ^ The actual content in the section.
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON a => FromJSON (Section a) where
  parseJSON = defaultParseJson "Section"

instance ToJSON a => ToJSON (Section a) where
  toEncoding = defaultToEncoding "Section"
  toJSON = defaultToJson "Section"
