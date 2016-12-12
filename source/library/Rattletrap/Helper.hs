module Rattletrap.Helper where

import Rattletrap.Json ()
import Rattletrap.Replay

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified System.IO as IO

encodeJson :: Replay -> ByteString.ByteString
encodeJson replay = Aeson.encode replay

encodeJsonFile :: Replay -> FilePath -> IO ()
encodeJsonFile replay file =
  IO.withBinaryFile file IO.WriteMode (encodeJsonHandle replay)

encodeJsonHandle :: Replay -> IO.Handle -> IO ()
encodeJsonHandle replay handle = ByteString.hPut handle (encodeJson replay)

encodeReplay :: Replay -> ByteString.ByteString
encodeReplay replay = Binary.runPut (putReplay replay)

encodeReplayFile :: Replay -> FilePath -> IO ()
encodeReplayFile replay file =
  IO.withBinaryFile file IO.WriteMode (encodeReplayHandle replay)

encodeReplayHandle :: Replay -> IO.Handle -> IO ()
encodeReplayHandle replay handle = ByteString.hPut handle (encodeReplay replay)

decodeJson :: ByteString.ByteString -> Either String Replay
decodeJson contents = Aeson.eitherDecode contents

decodeJsonFile :: FilePath -> IO (Either String Replay)
decodeJsonFile file = IO.withBinaryFile file IO.ReadMode decodeJsonHandle

decodeJsonHandle :: IO.Handle -> IO (Either String Replay)
decodeJsonHandle handle = do
  contents <- ByteString.hGetContents handle
  pure (decodeJson contents)

decodeReplay :: ByteString.ByteString -> Either String Replay
decodeReplay contents =
  case Binary.runGetOrFail getReplay contents of
    Left (_, _, message) -> fail message
    Right (_, _, replay) -> pure replay

decodeReplayFile :: FilePath -> IO (Either String Replay)
decodeReplayFile file = IO.withBinaryFile file IO.ReadMode decodeReplayHandle

decodeReplayHandle :: IO.Handle -> IO (Either String Replay)
decodeReplayHandle handle = do
  contents <- ByteString.hGetContents handle
  pure (decodeReplay contents)
