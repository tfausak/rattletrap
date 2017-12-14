{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.PrivateMatchSettingsAttribute
  ( PrivateMatchSettingsAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32
import Rattletrap.Type.Text

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { privateMatchSettingsAttributeMutators :: Text
  , privateMatchSettingsAttributeJoinableBy :: Word32
  , privateMatchSettingsAttributeMaxPlayers :: Word32
  , privateMatchSettingsAttributeGameName :: Text
  , privateMatchSettingsAttributePassword :: Text
  , privateMatchSettingsAttributeFlag :: Bool
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON PrivateMatchSettingsAttribute where
  parseJSON = defaultParseJson "PrivateMatchSettingsAttribute"

instance ToJSON PrivateMatchSettingsAttribute where
  toEncoding = defaultToEncoding "PrivateMatchSettingsAttribute"
  toJSON = defaultToJson "PrivateMatchSettingsAttribute"
