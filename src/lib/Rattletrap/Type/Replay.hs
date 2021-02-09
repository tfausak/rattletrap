{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replay
  ( FullReplay
  , Replay(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Content
import Rattletrap.Type.Header
import Rattletrap.Type.Section

type FullReplay = Replay Content

-- | A Rocket League replay.
data Replay content = Replay
  { replayHeader :: Section Header
  -- ^ This has most of the high-level metadata.
  , replayContent :: Section content
  -- ^ This has most of the low-level game data.
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Replay)
