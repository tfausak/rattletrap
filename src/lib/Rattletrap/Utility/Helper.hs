-- | This module provides helper functions for converting replays to and from
-- both their binary format and JSON.
module Rattletrap.Utility.Helper where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Exception.InvalidJson as InvalidJson
import qualified Rattletrap.Type.Replay as Replay
import qualified Rattletrap.Utility.Json as Json

-- | Parses a raw replay.
decodeReplayFile ::
  Bool ->
  Bool ->
  ByteString.ByteString ->
  Either ([String], Exception.SomeException) Replay.Replay
decodeReplayFile fast = ByteGet.run . Replay.byteGet fast

-- | Encodes a replay as JSON.
encodeReplayJson :: Replay.Replay -> LazyByteString.ByteString
encodeReplayJson = Json.encodePretty

-- | Parses a JSON replay.
decodeReplayJson ::
  ByteString.ByteString ->
  Either ([String], Exception.SomeException) Replay.Replay
decodeReplayJson =
  Bifunctor.first ((,) [] . Exception.toException . InvalidJson.InvalidJson)
    . Json.decode

-- | Encodes a raw replay.
encodeReplayFile :: Bool -> Replay.Replay -> LazyByteString.ByteString
encodeReplayFile fast = BytePut.toLazyByteString . Replay.bytePut fast
