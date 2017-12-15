{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Frame
  ( Frame(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32
import Rattletrap.Type.Replication

data Frame = Frame
  { frameTime :: Float32
  -- ^ Time in seconds since the beginning of the match.
  , frameDelta :: Float32
  -- ^ Time in seconds since the last frame. Usually about 0.03 since there
  -- are 30 frames per second.
  , frameReplications :: [Replication]
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Frame where
  parseJSON = defaultParseJson "Frame"

instance ToJSON Frame where
  toEncoding = defaultToEncoding "Frame"
  toJSON = defaultToJson "Frame"
