{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.LoadoutsOnlineAttribute
  ( LoadoutsOnlineAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.LoadoutOnlineAttribute

data LoadoutsOnlineAttribute = LoadoutsOnlineAttribute
  { loadoutsOnlineAttributeBlue :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeOrange :: LoadoutOnlineAttribute
  , loadoutsOnlineAttributeUnknown1 :: Bool
  , loadoutsOnlineAttributeUnknown2 :: Bool
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON LoadoutsOnlineAttribute where
  parseJSON = defaultParseJson "LoadoutsOnlineAttribute"

instance ToJSON LoadoutsOnlineAttribute where
  toEncoding = defaultToEncoding "LoadoutsOnlineAttribute"
  toJSON = defaultToJson "LoadoutsOnlineAttribute"
