{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.LoadoutsAttribute
  ( LoadoutsAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.LoadoutAttribute

data LoadoutsAttribute = LoadoutsAttribute
  { loadoutsAttributeBlue :: LoadoutAttribute
  , loadoutsAttributeOrange :: LoadoutAttribute
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON LoadoutsAttribute where
  parseJSON = defaultParseJson "LoadoutsAttribute"

instance ToJSON LoadoutsAttribute where
  toEncoding = defaultToEncoding "LoadoutsAttribute"
  toJSON = defaultToJson "LoadoutsAttribute"
