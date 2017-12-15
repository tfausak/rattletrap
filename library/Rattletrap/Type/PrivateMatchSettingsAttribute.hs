{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PrivateMatchSettingsAttribute
  ( PrivateMatchSettingsAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Text

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { privateMatchSettingsAttributeMutators :: Text
  , privateMatchSettingsAttributeJoinableBy :: Word32le
  , privateMatchSettingsAttributeMaxPlayers :: Word32le
  , privateMatchSettingsAttributeGameName :: Text
  , privateMatchSettingsAttributePassword :: Text
  , privateMatchSettingsAttributeFlag :: Bool
  } deriving (Eq, Ord, Show)

$(deriveJson ''PrivateMatchSettingsAttribute)
