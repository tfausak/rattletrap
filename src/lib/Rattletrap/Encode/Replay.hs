module Rattletrap.Encode.Replay
  ( putReplay
  )
where

import Rattletrap.Encode.Content
import Rattletrap.Encode.Header
import Rattletrap.Encode.Section
import Rattletrap.Type.Replay

import qualified Data.Binary as Binary

-- | Generates a raw replay. Use this with 'Data.Binary.Put.runPut'.
--
-- @
-- let bytes = 'Data.Binary.Put.runPut' ('putReplay' replay)
-- @
putReplay :: FullReplay -> Binary.Put
putReplay replay = do
  putSection putHeader (replayHeader replay)
  putSection putContent (replayContent replay)
