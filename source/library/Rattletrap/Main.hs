module Rattletrap.Main where

import Rattletrap.Helper
import Rattletrap.Version

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Version as Version
import qualified System.Environment as Environment

-- | Gets command-line arguments and passes them to 'mainWithArgs'.
main :: IO ()
main = do
  args <- Environment.getArgs
  mainWithArgs args

-- | Runs the main command-line interface. Can be used in one of three modes:
--
-- 1. @mainWithArgs ["version"]@: Prints out the version number and exits.
--    Mostly useful for debugging.
-- 2. @mainWithArgs ["decode"]@: Parses a raw replay into JSON. By default
--    this reads from STDIN and writes to STDOUT. To read from a file, pass a
--    second argument like @mainWithArgs ["decode", "input.replay"]@. To write
--    to a file, pass a third argument like
--    @mainWithArgs ["decode", "input.replay", "output.json"]@.
-- 3. @mainWithArgs ["encode"]@: Generates a raw replay from JSON. The
--    handling of input and output is the same as decoding.
mainWithArgs :: [String] -> IO ()
mainWithArgs args =
  case args of
    ["version"] -> putStrLn (Version.showVersion version)
    "decode":files -> do
      (getInput, putOutput) <- getIO files
      input <- getInput
      case decodeReplay input of
        Left message -> fail message
        Right replay -> do
          let output = encodeJson replay
          putOutput output
    "encode":files -> do
      (getInput, putOutput) <- getIO files
      input <- getInput
      case decodeJson input of
        Left message -> fail ("could not parse JSON: " ++ message)
        Right replay -> do
          let output = encodeReplay replay
          putOutput output
    _ -> fail ("unexpected arguments " ++ show args)

getIO :: [FilePath]
      -> IO (IO ByteString.ByteString, ByteString.ByteString -> IO ())
getIO files =
  case files of
    [] -> pure (ByteString.getContents, ByteString.putStr)
    [i] -> pure (ByteString.readFile i, ByteString.putStr)
    [i, o] -> pure (ByteString.readFile i, ByteString.writeFile o)
    _ -> fail ("unexpected arguments " ++ show files)
