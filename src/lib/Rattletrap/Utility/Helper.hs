-- | This module provides helper functions for converting replays to and from
-- both their binary format and JSON.
module Rattletrap.Utility.Helper
  ( decodeReplayFile
  , encodeReplayJson
  , decodeReplayJson
  , encodeReplayFile
  ) where

import qualified Rattletrap.Type.Content as Content
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Replay as Replay
import qualified Rattletrap.Type.Section as Section
import qualified Rattletrap.ByteGet as ByteGet

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes

-- | Parses a raw replay.
decodeReplayFile :: Bool -> Bytes.ByteString -> Either String Replay.FullReplay
decodeReplayFile = ByteGet.run . Replay.byteGet

-- | Encodes a replay as JSON.
encodeReplayJson :: Replay.FullReplay -> Bytes.ByteString
encodeReplayJson = LazyBytes.toStrict . Json.encodePretty' Json.defConfig
  { Json.confCompare = compare
  , Json.confIndent = Json.Spaces 2
  , Json.confTrailingNewline = True
  }

-- | Parses a JSON replay.
decodeReplayJson :: Bytes.ByteString -> Either String Replay.FullReplay
decodeReplayJson = Json.eitherDecodeStrict'

-- | Encodes a raw replay.
encodeReplayFile :: Bool -> Replay.FullReplay -> Bytes.ByteString
encodeReplayFile fast replay =
  BytePut.toByteString . Replay.bytePut $ if fast
    then replay { Replay.content = Section.create Content.bytePut Content.empty }
    else replay
