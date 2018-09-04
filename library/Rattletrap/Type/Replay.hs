{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replay
  ( Replay(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Content
import Rattletrap.Type.Header
import Rattletrap.Type.Section

-- | A Rocket League replay.
data Replay = Replay
  { replayHeader :: Section Header
  -- ^ This has most of the high-level metadata.
  , replayContent :: Section Content
  -- ^ This has most of the low-level game data.
  } deriving (Eq, Ord, Show)

$(deriveJson ''Replay)
