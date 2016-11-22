module Rattletrap.Replay where

import Rattletrap.Content
import Rattletrap.Header
import Rattletrap.Primitive.Section

import qualified Data.Binary as Binary

-- | A Rocket League replay.
data Replay = Replay
  { replayHeader :: Section Header
  -- ^ This has most of the high-level metadata.
  , replayContent :: Section Content
  -- ^ This has most of the low-level game data.
  } deriving (Eq, Ord, Show)

-- | Parses a raw replay. Use this with 'Data.Binary.Get.runGet'.
--
-- @
-- let replay = 'Data.Binary.Get.runGet' 'getReplay' bytes
-- @
getReplay :: Binary.Get Replay
getReplay = do
  header <- getSection getHeader
  let version = getVersion (sectionBody header)
  let numFrames = getNumFrames (sectionBody header)
  content <- getSection (getContent version numFrames)
  pure (Replay header content)

-- | Generates a raw replay. Use this with 'Data.Binary.Put.runPut'.
--
-- @
-- let bytes = 'Data.Binary.Put.runPut' ('putReplay' replay)
-- @
putReplay :: Replay -> Binary.Put
putReplay replay = do
  putSection putHeader (replayHeader replay)
  putSection putContent (replayContent replay)
