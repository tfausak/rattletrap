module Rattletrap.Main where

import Rattletrap.Replay

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified System.Environment as Environment

main :: IO ()
main = do
  args <- Environment.getArgs
  mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs args =
  case args of
    [replayFile, jsonFile] -> do
      input <- ByteString.readFile replayFile
      let replay = Binary.runGet getReplay input
      let config = Aeson.defConfig {Aeson.confCompare = compare}
      let json = Aeson.encodePretty' config replay
      ByteString.writeFile jsonFile json
    _ -> fail ("unexpected arguments " ++ show args)
