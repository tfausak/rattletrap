{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.KeyFrame
  ( KeyFrame(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32
import Rattletrap.Type.Word32

data KeyFrame = KeyFrame
  { keyFrameTime :: Float32
  -- ^ When this key frame occurs, in seconds.
  , keyFrameFrame :: Word32
  -- ^ The frame number of this key frame, starting from 0.
  , keyFramePosition :: Word32
  -- ^ The bit position of this key frame in the stream.
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON KeyFrame where
  parseJSON = defaultParseJson "KeyFrame"

instance ToJSON KeyFrame where
  toEncoding = defaultToEncoding "KeyFrame"
  toJSON = defaultToJson "KeyFrame"
