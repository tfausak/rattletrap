module Rattletrap.Main where

import Rattletrap.Replay

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified System.Environment as Environment

main :: IO ()
main = do
  args <- Environment.getArgs
  mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs args =
  case args of
    ["replay2json", replayFile, jsonFile] -> do
      input <- ByteString.readFile replayFile
      let replay = Binary.runGet getReplay input
      let config = Aeson.defConfig {Aeson.confCompare = compare}
      let output = Aeson.encodePretty' config replay
      ByteString.writeFile jsonFile output
    ["json2replay", jsonFile, replayfile] -> do
      input <- ByteString.readFile jsonFile
      case Aeson.eitherDecode input of
        Left message -> fail ("could not parse JSON: " ++ message)
        Right replay -> do
          let output = Binary.runPut (putReplay replay)
          ByteString.writeFile replayfile output
    _ -> fail ("unexpected arguments " ++ show args)
