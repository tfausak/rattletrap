module Rattletrap.Replay where

import Rattletrap.Word32

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyByteString

data Replay = Replay
  { replayHeaderSize :: Word32
  , replayHeaderCrc :: Word32
  , replayHeader :: LazyByteString.ByteString
  , replayContentSize :: Word32
  , replayContentCrc :: Word32
  , replayContent :: LazyByteString.ByteString
  , replayFooter :: LazyByteString.ByteString
  }

getReplay :: Binary.Get Replay
getReplay = do
  headerSize <- getWord32
  headerCrc <- getWord32
  header <- Binary.getLazyByteString (fromIntegral (word32Unpack headerSize))
  contentSize <- getWord32
  contentCrc <- getWord32
  content <- Binary.getLazyByteString (fromIntegral (word32Unpack contentSize))
  footer <- Binary.getRemainingLazyByteString
  pure
    Replay
    { replayHeaderSize = headerSize
    , replayHeaderCrc = headerCrc
    , replayHeader = header
    , replayContentSize = contentSize
    , replayContentCrc = contentCrc
    , replayContent = content
    , replayFooter = footer
    }

putReplay :: Replay -> Binary.Put
putReplay replay = do
  putWord32 (replayHeaderSize replay)
  putWord32 (replayHeaderCrc replay)
  Binary.putLazyByteString (replayHeader replay)
  putWord32 (replayContentSize replay)
  putWord32 (replayContentCrc replay)
  Binary.putLazyByteString (replayContent replay)
  Binary.putLazyByteString (replayFooter replay)
