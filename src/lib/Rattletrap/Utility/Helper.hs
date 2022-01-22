-- | This module provides helper functions for converting replays to and from
-- both their binary format and JSON.
module Rattletrap.Utility.Helper where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Exception.InvalidJson as InvalidJson
import qualified Rattletrap.Type.Content as Content
import qualified Rattletrap.Type.Replay as Replay
import qualified Rattletrap.Type.Section as Section
import qualified Rattletrap.Vendor.Argo as Argo

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString

-- | Parses a raw replay.
decodeReplayFile
  :: Bool
  -> Bool
  -> ByteString.ByteString
  -> Either ([String], Exception.SomeException) Replay.Replay
decodeReplayFile fast = ByteGet.run . Replay.byteGet fast

-- | Encodes a replay as JSON.
encodeReplayJson :: Replay.Replay -> LazyByteString.ByteString
encodeReplayJson = Builder.toLazyByteString . Argo.encodeWith Argo.Tab

-- | Parses a JSON replay.
decodeReplayJson
  :: ByteString.ByteString
  -> Either ([String], Exception.SomeException) Replay.Replay
decodeReplayJson =
  Bifunctor.first ((,) [] . Exception.toException . InvalidJson.InvalidJson)
    . Argo.decode

-- | Encodes a raw replay.
encodeReplayFile :: Bool -> Replay.Replay -> LazyByteString.ByteString
encodeReplayFile fast replay =
  BytePut.toLazyByteString . Replay.bytePut $ if fast
    then replay
      { Replay.content = Section.create Content.bytePut Content.empty
      }
    else replay
