{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.FlaggedIntAttribute
  ( FlaggedIntAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32

data FlaggedIntAttribute = FlaggedIntAttribute
  { flaggedIntAttributeFlag :: Bool
  , flaggedIntAttributeInt :: Int32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON FlaggedIntAttribute where
  parseJSON = defaultParseJson "FlaggedIntAttribute"

instance ToJSON FlaggedIntAttribute where
  toEncoding = defaultToEncoding "FlaggedIntAttribute"
  toJSON = defaultToJson "FlaggedIntAttribute"
