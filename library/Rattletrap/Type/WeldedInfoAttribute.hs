{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.WeldedInfoAttribute
  ( WeldedInfoAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32
import Rattletrap.Type.Vector
import Rattletrap.Type.Float32
import Rattletrap.Type.Int8Vector

data WeldedInfoAttribute = WeldedInfoAttribute
  { weldedInfoAttributeActive :: Bool
  , weldedInfoAttributeActorId :: Int32
  , weldedInfoAttributeOffset :: Vector
  , weldedInfoAttributeMass :: Float32
  , weldedInfoAttributeRotation :: Int8Vector
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON WeldedInfoAttribute where
  parseJSON = defaultParseJson "WeldedInfoAttribute"

instance ToJSON WeldedInfoAttribute where
  toEncoding = defaultToEncoding "WeldedInfoAttribute"
  toJSON = defaultToJson "WeldedInfoAttribute"
