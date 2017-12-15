-- | This module provides helper functions for converting replays to and from
-- both their binary format and JSON.
module Rattletrap.Utility.Helper
  ( decodeReplay
  , encodeJson
  , decodeJson
  , encodeReplay
  ) where

import Rattletrap.Type.Replay
import Rattletrap.Decode.Replay
import Rattletrap.Encode.Replay

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified System.IO as IO

-- | Parses a raw replay.
decodeReplay :: ByteString.ByteString -> Either String Replay
decodeReplay contents = case Binary.runGetOrFail getReplay contents of
  Left (_, _, message) -> fail message
  Right (_, _, replay) -> pure replay

-- | Encodes a replay as JSON.
encodeJson :: Replay -> ByteString.ByteString
encodeJson = Aeson.encode

-- | Parses a JSON replay.
decodeJson :: ByteString.ByteString -> Either String Replay
decodeJson = Aeson.eitherDecode

-- | Encodes a raw replay.
encodeReplay :: Replay -> ByteString.ByteString
encodeReplay replay = Binary.runPut (putReplay replay)
