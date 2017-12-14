module Rattletrap.Main where

import Rattletrap.Utility.Helper
import Rattletrap.Utility.Version

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
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
mainWithArgs args = case args of
  ["version"] -> putStrLn (Version.showVersion version)
  action:files -> do
    (getInput, putOutput) <- getIO files
    input <- getInput
    output <- case action of
      "decode" -> case decodeReplay input of
        Left message -> fail message
        Right replay -> pure (encodeJson replay)
      "encode" -> case decodeJson input of
        Left message -> fail message
        Right replay -> pure (encodeReplay replay)
      _ -> fail ("unknown action: " ++ show action)
    putOutput output
  _ -> fail ("unknown arguments: " ++ show args)

getIO
  :: [FilePath] -> IO (IO ByteString.ByteString, ByteString.ByteString -> IO ())
getIO files = case files of
  [] -> pure (ByteString.getContents, ByteString.putStr)
  [i] -> pure (readUrlOrFile i, ByteString.putStr)
  [i, o] -> pure (readUrlOrFile i, ByteString.writeFile o)
  _ -> fail ("unknown files: " ++ show files)

readUrlOrFile :: FilePath -> IO ByteString.ByteString
readUrlOrFile i = case Client.parseUrlThrow i of
  Just request -> do
    manager <- Client.newTlsManager
    response <- Client.httpLbs request manager
    pure (Client.responseBody response)
  Nothing -> ByteString.readFile i
