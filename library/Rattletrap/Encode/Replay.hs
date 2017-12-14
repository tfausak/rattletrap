module Rattletrap.Encode.Replay
  ( putReplay
  ) where

import Rattletrap.Type.Replay
import Rattletrap.Encode.Content
import Rattletrap.Encode.Header
import Rattletrap.Encode.Section

import qualified Data.Binary as Binary

-- | Generates a raw replay. Use this with 'Data.Binary.Put.runPut'.
--
-- @
-- let bytes = 'Data.Binary.Put.runPut' ('putReplay' replay)
-- @
putReplay :: Replay -> Binary.Put
putReplay replay = do
  putSection putHeader (replayHeader replay)
  putSection putContent (replayContent replay)
