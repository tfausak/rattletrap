module Rattletrap.Replay where

import Rattletrap.Content
import Rattletrap.Header
import Rattletrap.Primitive.Section

import qualified Data.Binary as Binary

data Replay = Replay
  { replayHeader :: Section Header
  , replayContent :: Section Content
  } deriving (Eq, Ord, Show)

getReplay :: Binary.Get Replay
getReplay = do
  header <- getSection getHeader
  let version = getVersion (sectionBody header)
  let numFrames = getNumFrames (sectionBody header)
  content <- getSection (getContent version numFrames)
  pure (Replay header content)

putReplay :: Replay -> Binary.Put
putReplay replay = do
  putSection putHeader (replayHeader replay)
  putSection putContent (replayContent replay)
