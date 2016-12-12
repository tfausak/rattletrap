-- | This module provides helper functions for converting replays to and from
-- both their binary format and JSON.
module Rattletrap.Helper where

import Rattletrap.Json ()
import Rattletrap.Replay

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified System.IO as IO

-- * Lazy byte strings
-- | Parses a raw replay.
decodeReplay :: ByteString.ByteString -> Either String Replay
decodeReplay contents =
  case Binary.runGetOrFail getReplay contents of
    Left (_, _, message) -> fail message
    Right (_, _, replay) -> pure replay

-- | Encodes a replay as JSON.
encodeJson :: Replay -> ByteString.ByteString
encodeJson replay = Aeson.encode replay

-- | Parses a JSON replay.
decodeJson :: ByteString.ByteString -> Either String Replay
decodeJson contents = Aeson.eitherDecode contents

-- | Encodes a raw replay.
encodeReplay :: Replay -> ByteString.ByteString
encodeReplay replay = Binary.runPut (putReplay replay)

-- * File paths
-- | 'decodeReplay'
decodeReplayFile :: FilePath -> IO (Either String Replay)
decodeReplayFile file = IO.withBinaryFile file IO.ReadMode decodeReplayHandle

-- | 'encodeJson'
encodeJsonFile :: Replay -> FilePath -> IO ()
encodeJsonFile replay file =
  IO.withBinaryFile file IO.WriteMode (encodeJsonHandle replay)

-- | 'decodeJson'
decodeJsonFile :: FilePath -> IO (Either String Replay)
decodeJsonFile file = IO.withBinaryFile file IO.ReadMode decodeJsonHandle

-- | 'encodeReplay'
encodeReplayFile :: Replay -> FilePath -> IO ()
encodeReplayFile replay file =
  IO.withBinaryFile file IO.WriteMode (encodeReplayHandle replay)

-- * Handles
-- | 'decodeReplay'
decodeReplayHandle :: IO.Handle -> IO (Either String Replay)
decodeReplayHandle handle = do
  contents <- ByteString.hGetContents handle
  pure (decodeReplay contents)

-- | 'encodeJson'
encodeJsonHandle :: Replay -> IO.Handle -> IO ()
encodeJsonHandle replay handle = ByteString.hPut handle (encodeJson replay)

-- | 'decodeJson'
decodeJsonHandle :: IO.Handle -> IO (Either String Replay)
decodeJsonHandle handle = do
  contents <- ByteString.hGetContents handle
  pure (decodeJson contents)

-- | 'encodeReplay'
encodeReplayHandle :: Replay -> IO.Handle -> IO ()
encodeReplayHandle replay handle = ByteString.hPut handle (encodeReplay replay)
