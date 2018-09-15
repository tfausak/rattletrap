-- | This module provides helper functions for converting replays to and from
-- both their binary format and JSON.
module Rattletrap.Utility.Helper
  ( decodeReplayFile
  , encodeReplayJson
  , decodeReplayJson
  , encodeReplayFile
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Replay
import Rattletrap.Encode.Replay
import Rattletrap.Type.Replay

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes

-- | Parses a raw replay.
decodeReplayFile :: Bytes.ByteString -> Either String Replay
decodeReplayFile = runDecode decodeReplay

-- | Encodes a replay as JSON.
encodeReplayJson :: Replay -> Bytes.ByteString
encodeReplayJson = LazyBytes.toStrict . Json.encodePretty' Json.defConfig
  { Json.confCompare = compare
  , Json.confIndent = Json.Spaces 2
  , Json.confTrailingNewline = True
  }

-- | Parses a JSON replay.
decodeReplayJson :: Bytes.ByteString -> Either String Replay
decodeReplayJson = Json.eitherDecodeStrict'

-- | Encodes a raw replay.
encodeReplayFile :: Replay -> Bytes.ByteString
encodeReplayFile replay =
  LazyBytes.toStrict (Binary.runPut (putReplay replay))
