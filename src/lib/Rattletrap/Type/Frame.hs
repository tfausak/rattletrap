{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Frame
  ( Frame(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Float32le
import Rattletrap.Type.Replication

data Frame = Frame
  { frameTime :: Float32le
  -- ^ Time in seconds since the beginning of the match.
  , frameDelta :: Float32le
  -- ^ Time in seconds since the last frame. Usually about 0.03 since there
  -- are 30 frames per second.
  , frameReplications :: [Replication]
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Frame)
