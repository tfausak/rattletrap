-- | This module provides helper functions for converting replays to and from
-- both their binary format and JSON.
module Rattletrap.Utility.Helper
  ( decodeReplayFile
  , encodeReplayJson
  , decodeReplayJson
  , encodeReplayFile
  ) where

import Rattletrap.Decode.Replay
import Rattletrap.Encode.Replay
import Rattletrap.Type.Replay

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

-- | Parses a raw replay.
decodeReplayFile :: LazyBytes.ByteString -> Either String Replay
decodeReplayFile contents = case Binary.runGetOrFail decodeReplay contents of
  Left (_, _, message) -> fail message
  Right (_, _, replay) -> pure replay

-- | Encodes a replay as JSON.
encodeReplayJson :: Replay -> LazyBytes.ByteString
encodeReplayJson = Json.encodePretty' Json.defConfig
  { Json.confCompare = compare
  , Json.confIndent = Json.Spaces 2
  , Json.confTrailingNewline = True
  }

-- | Parses a JSON replay.
decodeReplayJson :: LazyBytes.ByteString -> Either String Replay
decodeReplayJson = Json.eitherDecode

-- | Encodes a raw replay.
encodeReplayFile :: Replay -> LazyBytes.ByteString
encodeReplayFile replay = Binary.runPut (putReplay replay)
