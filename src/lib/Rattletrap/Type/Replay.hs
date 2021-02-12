{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replay where

import Rattletrap.Type.Common
import Rattletrap.Type.Content
import Rattletrap.Type.Header
import Rattletrap.Type.Section
import Rattletrap.Encode.Content

import qualified Data.Binary as Binary

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

-- | Generates a raw replay. Use this with 'Data.Binary.Put.runPut'.
--
-- @
-- let bytes = 'Data.Binary.Put.runPut' ('putReplay' replay)
-- @
putReplay :: FullReplay -> Binary.Put
putReplay replay = do
  putSection putHeader (replayHeader replay)
  putSection putContent (replayContent replay)
