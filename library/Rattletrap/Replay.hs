module Rattletrap.Replay where

import Rattletrap.Content
import Rattletrap.Crc
import Rattletrap.Dictionary
import Rattletrap.Header
import Rattletrap.Int32
import Rattletrap.Property
import Rattletrap.PropertyValue
import Rattletrap.Text
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
  let majorVersion = fromIntegral (word32Value (headerEngineVersion header))
  let minorVersion = fromIntegral (word32Value (headerLicenseeVersion header))
  let version = (majorVersion, minorVersion)
  let numFrames =
        case lookup
               (stringToText "NumFrames")
               (dictionaryValue (headerProperties header)) of
          Just (Just (Property _ _ (IntProperty int32))) ->
            fromIntegral (int32Value int32)
          _ -> 0
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
