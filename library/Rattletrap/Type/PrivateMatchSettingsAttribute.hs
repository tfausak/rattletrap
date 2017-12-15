{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PrivateMatchSettingsAttribute
  ( PrivateMatchSettingsAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { privateMatchSettingsAttributeMutators :: Str
  , privateMatchSettingsAttributeJoinableBy :: Word32le
  , privateMatchSettingsAttributeMaxPlayers :: Word32le
  , privateMatchSettingsAttributeGameName :: Str
  , privateMatchSettingsAttributePassword :: Str
  , privateMatchSettingsAttributeFlag :: Bool
  } deriving (Eq, Ord, Show)

$(deriveJson ''PrivateMatchSettingsAttribute)
