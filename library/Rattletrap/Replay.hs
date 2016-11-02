module Rattletrap.Replay where

import Rattletrap.Content
import Rattletrap.Crc
import Rattletrap.Header
import Rattletrap.Word32

import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

data Replay = Replay
  { replayHeader :: Header
  , replayContent :: Content
  } deriving (Eq, Ord, Show)

getReplay :: Binary.Get Replay
getReplay = do
  _headerSize <- getWord32
  _headerCrc <- getWord32
  header <- getHeader
  _contentSize <- getWord32
  _contentCrc <- getWord32
  let version = getVersion header
  let numFrames = getNumFrames header
  content <- getContent version numFrames
  pure Replay {replayHeader = header, replayContent = content}

putReplay :: Replay -> Binary.Put
putReplay replay = do
  let header = Binary.runPut (putHeader (replayHeader replay))
  let headerSize = ByteString.length header
  let headerCrc = getCrc32 header
  putWord32 (Word32 (fromIntegral headerSize))
  putWord32 (Word32 headerCrc)
  Binary.putLazyByteString header
  let content = Binary.runPut (putContent (replayContent replay))
  let contentSize = ByteString.length content
  let contentCrc = getCrc32 content
  putWord32 (Word32 (fromIntegral contentSize))
  putWord32 (Word32 contentCrc)
  Binary.putLazyByteString content
