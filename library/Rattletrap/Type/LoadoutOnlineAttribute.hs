{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.LoadoutOnlineAttribute
  ( LoadoutOnlineAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.ProductAttribute

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[ProductAttribute]]
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON LoadoutOnlineAttribute where
  parseJSON = defaultParseJson "LoadoutOnlineAttribute"

instance ToJSON LoadoutOnlineAttribute where
  toEncoding = defaultToEncoding "LoadoutOnlineAttribute"
  toJSON = defaultToJson "LoadoutOnlineAttribute"
