module Rattletrap.Replay where

import Rattletrap.Content
import Rattletrap.Dictionary
import Rattletrap.Header
import Rattletrap.Int32
import Rattletrap.Property
import Rattletrap.PropertyValue
import Rattletrap.Text
import Rattletrap.Word32

import qualified Data.Binary as Binary

data Replay = Replay
  { replayHeaderSize :: Word32
  , replayHeaderCrc :: Word32
  , replayHeader :: Header
  , replayContentSize :: Word32
  , replayContentCrc :: Word32
  , replayContent :: Content
  } deriving (Eq, Ord, Show)

getReplay :: Binary.Get Replay
getReplay = do
  headerSize <- getWord32
  headerCrc <- getWord32
  header <- getHeader
  contentSize <- getWord32
  contentCrc <- getWord32
  let numFrames =
        case lookup
               (stringToText "NumFrames")
               (dictionaryValue (headerProperties header)) of
          Just (Just (Property _ _ (IntProperty int32))) ->
            fromIntegral (int32Value int32)
          _ -> 0
  content <- getContent numFrames
  pure
    Replay
    { replayHeaderSize = headerSize
    , replayHeaderCrc = headerCrc
    , replayHeader = header
    , replayContentSize = contentSize
    , replayContentCrc = contentCrc
    , replayContent = content
    }

putReplay :: Replay -> Binary.Put
putReplay replay = do
  putWord32 (replayHeaderSize replay)
  putWord32 (replayHeaderCrc replay)
  putHeader (replayHeader replay)
  putWord32 (replayContentSize replay)
  putWord32 (replayContentCrc replay)
  putContent (replayContent replay)
