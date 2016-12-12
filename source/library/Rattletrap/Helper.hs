module Rattletrap.Helper where

import Rattletrap.Json ()
import Rattletrap.Replay

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

encodeJson :: Replay -> ByteString.ByteString
encodeJson replay = Aeson.encode replay

encodeJsonFile :: Replay -> FilePath -> IO ()
encodeJsonFile replay file = ByteString.writeFile file (encodeJson replay)

encodeReplay :: Replay -> ByteString.ByteString
encodeReplay replay = Binary.runPut (putReplay replay)

encodeReplayFile :: Replay -> FilePath -> IO ()
encodeReplayFile replay file = ByteString.writeFile file (encodeReplay replay)

decodeJson :: ByteString.ByteString -> Either String Replay
decodeJson contents = Aeson.eitherDecode contents

decodeJsonFile :: FilePath -> IO (Either String Replay)
decodeJsonFile file = do
  contents <- ByteString.readFile file
  pure (decodeJson contents)

decodeReplay :: ByteString.ByteString -> Either String Replay
decodeReplay contents =
  case Binary.runGetOrFail getReplay contents of
    Left (_, _, message) -> fail message
    Right (_, _, replay) -> pure replay

decodeReplayFile :: FilePath -> IO (Either String Replay)
decodeReplayFile file = do
  contents <- ByteString.readFile file
  pure (decodeReplay contents)
