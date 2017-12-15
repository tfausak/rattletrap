{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.ExplosionAttribute
  ( ExplosionAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32
import Rattletrap.Type.Vector

data ExplosionAttribute = ExplosionAttribute
  { explosionAttributeActorId :: Int32
  , explosionAttributeLocation :: Vector
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON ExplosionAttribute where
  parseJSON = defaultParseJson "ExplosionAttribute"

instance ToJSON ExplosionAttribute where
  toEncoding = defaultToEncoding "ExplosionAttribute"
  toJSON = defaultToJson "ExplosionAttribute"
