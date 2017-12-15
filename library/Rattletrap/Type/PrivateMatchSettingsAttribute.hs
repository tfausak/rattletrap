{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PrivateMatchSettingsAttribute
  ( PrivateMatchSettingsAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Str

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { privateMatchSettingsAttributeMutators :: Str
  , privateMatchSettingsAttributeJoinableBy :: Word32le
  , privateMatchSettingsAttributeMaxPlayers :: Word32le
  , privateMatchSettingsAttributeGameName :: Str
  , privateMatchSettingsAttributePassword :: Str
  , privateMatchSettingsAttributeFlag :: Bool
  } deriving (Eq, Ord, Show)

$(deriveJson ''PrivateMatchSettingsAttribute)
