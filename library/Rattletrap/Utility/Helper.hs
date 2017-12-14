-- | This module provides helper functions for converting replays to and from
-- both their binary format and JSON.
module Rattletrap.Utility.Helper
  ( decodeReplay
  , encodeJson
  , decodeJson
  , encodeReplay
  , decodeReplayFile
  , encodeJsonFile
  , decodeJsonFile
  , encodeReplayFile
  , decodeReplayHandle
  , encodeJsonHandle
  , decodeJsonHandle
  , encodeReplayHandle
  ) where

import Rattletrap.Type.Replay
import Rattletrap.Decode.Replay
import Rattletrap.Encode.Replay

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified System.IO as IO

-- * Lazy byte strings
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

-- * File paths
-- | 'decodeReplay'
{-# DEPRECATED decodeReplayFile "use Data.ByteString.Lazy.readFile"#-}
decodeReplayFile :: FilePath -> IO (Either String Replay)
decodeReplayFile file = IO.withBinaryFile file IO.ReadMode decodeReplayHandle

-- | 'encodeJson'
{-# DEPRECATED encodeJsonFile "use Data.ByteString.Lazy.writeFile"#-}
encodeJsonFile :: Replay -> FilePath -> IO ()
encodeJsonFile replay file =
  IO.withBinaryFile file IO.WriteMode (encodeJsonHandle replay)

-- | 'decodeJson'
{-# DEPRECATED decodeJsonFile "use Data.ByteString.Lazy.readFile"#-}
decodeJsonFile :: FilePath -> IO (Either String Replay)
decodeJsonFile file = IO.withBinaryFile file IO.ReadMode decodeJsonHandle

-- | 'encodeReplay'
{-# DEPRECATED encodeReplayFile "use Data.ByteString.Lazy.writeFile"#-}
encodeReplayFile :: Replay -> FilePath -> IO ()
encodeReplayFile replay file =
  IO.withBinaryFile file IO.WriteMode (encodeReplayHandle replay)

-- * Handles
-- | 'decodeReplay'
{-# DEPRECATED decodeReplayHandle "use Data.ByteString.Lazy.hGetContents"#-}
decodeReplayHandle :: IO.Handle -> IO (Either String Replay)
decodeReplayHandle handle = do
  contents <- ByteString.hGetContents handle
  pure (decodeReplay contents)

-- | 'encodeJson'
{-# DEPRECATED encodeJsonHandle "use Data.ByteString.Lazy.hPut"#-}
encodeJsonHandle :: Replay -> IO.Handle -> IO ()
encodeJsonHandle replay handle = ByteString.hPut handle (encodeJson replay)

-- | 'decodeJson'
{-# DEPRECATED decodeJsonHandle "use Data.ByteString.Lazy.hGetContents"#-}
decodeJsonHandle :: IO.Handle -> IO (Either String Replay)
decodeJsonHandle handle = do
  contents <- ByteString.hGetContents handle
  pure (decodeJson contents)

-- | 'encodeReplay'
{-# DEPRECATED encodeReplayHandle "use Data.ByteString.Lazy.hPut"#-}
encodeReplayHandle :: Replay -> IO.Handle -> IO ()
encodeReplayHandle replay handle = ByteString.hPut handle (encodeReplay replay)
