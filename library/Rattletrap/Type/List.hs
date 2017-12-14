{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.List
  ( List(..)
  ) where

import Rattletrap.Type.Common

newtype List a = List
  { listValue :: [a]
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON a => FromJSON (List a) where
  parseJSON = defaultParseJson "List"

instance ToJSON a => ToJSON (List a) where
  toEncoding = defaultToEncoding "List"
  toJSON = defaultToJson "List"
