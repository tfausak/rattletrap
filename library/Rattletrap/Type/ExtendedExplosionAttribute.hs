{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.ExtendedExplosionAttribute
  ( ExtendedExplosionAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32
import Rattletrap.Type.Vector

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeActorId :: Int32
  , extendedExplosionAttributeLocation :: Vector
  , extendedExplosionAttributeUnknown1 :: Bool
  , extendedExplosionAttributeUnknown2 :: Int32
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON ExtendedExplosionAttribute where
  parseJSON = defaultParseJson "ExtendedExplosionAttribute"

instance ToJSON ExtendedExplosionAttribute where
  toEncoding = defaultToEncoding "ExtendedExplosionAttribute"
  toJSON = defaultToJson "ExtendedExplosionAttribute"
