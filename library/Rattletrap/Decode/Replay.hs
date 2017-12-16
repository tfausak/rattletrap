module Rattletrap.Decode.Replay
  ( getReplay
  ) where

import Rattletrap.Decode.Content
import Rattletrap.Decode.Header
import Rattletrap.Decode.Section
import Rattletrap.Type.Header
import Rattletrap.Type.Replay
import Rattletrap.Type.Section

import qualified Data.Binary as Binary

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
  let maxChannels = getMaxChannels (sectionBody header)
  content <- getSection (decodeContent version numFrames maxChannels)
  pure (Replay header content)
