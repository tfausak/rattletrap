module Rattletrap.Helper where

import Rattletrap.Replay

import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as ByteString

decodeReplay :: ByteString.ByteString -> Either String Replay
decodeReplay contents =
  case Binary.runGetOrFail getReplay contents of
    Left (_, _, message) -> fail message
    Right (_, _, replay) -> pure replay

decodeReplayFile :: FilePath -> IO (Either String Replay)
decodeReplayFile file = do
  contents <- ByteString.readFile file
  pure (decodeReplay contents)
