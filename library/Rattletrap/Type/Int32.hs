{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Int32
  ( Int32(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Int as Int

newtype Int32 = Int32
  { int32Value :: Int.Int32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Int32 where
  parseJSON = defaultParseJson "Int32"

instance ToJSON Int32 where
  toEncoding = defaultToEncoding "Int32"
  toJSON = defaultToJson "Int32"
