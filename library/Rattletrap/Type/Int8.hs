{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Int8
  ( Int8(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Int as Int

newtype Int8 = Int8
  { int8Value :: Int.Int8
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Int8 where
  parseJSON = defaultParseJson "Int8"

instance ToJSON Int8 where
  toEncoding = defaultToEncoding "Int8"
  toJSON = defaultToJson "Int8"
