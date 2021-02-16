module Rattletrap.Console.Main
  ( main
  , rattletrap
  ) where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Json
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Paths_rattletrap as This
import qualified Rattletrap.Console.Config as Config
import qualified Rattletrap.Console.Mode as Mode
import qualified Rattletrap.Console.Option as Option
import qualified Rattletrap.Type.Replay as Replay
import qualified Rattletrap.Utility.Helper as Rattletrap
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

main :: IO ()
main = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  rattletrap name arguments

rattletrap :: String -> [String] -> IO ()
rattletrap name arguments = do
  config <- getConfig arguments
  Monad.when (Config.help config) $ do
    IO.hPutStr IO.stderr $
      Console.usageInfo (unwords [name, "version", version]) Option.all
    Exit.exitFailure
  Monad.when (Config.version config) $ do
    IO.hPutStrLn IO.stderr version
    Exit.exitFailure
  input <- getInput config
  let decode = getDecoder config
  replay <- either fail pure (decode input)
  let encode = getEncoder config
  putOutput config (encode replay)

getDecoder :: Config.Config -> Bytes.ByteString -> Either String Replay.FullReplay
getDecoder config = case Config.getMode config of
  Mode.Decode -> Rattletrap.decodeReplayFile $ Config.fast config
  Mode.Encode -> Rattletrap.decodeReplayJson

getEncoder :: Config.Config -> Replay.FullReplay -> Bytes.ByteString
getEncoder config = case Config.getMode config of
  Mode.Decode -> if Config.compact config
    then LazyBytes.toStrict . Json.encode
    else Rattletrap.encodeReplayJson
  Mode.Encode -> Rattletrap.encodeReplayFile $ Config.fast config

getInput :: Config.Config -> IO Bytes.ByteString
getInput config = case Config.input config of
  Nothing -> Bytes.getContents
  Just fileOrUrl -> case Client.parseUrlThrow fileOrUrl of
    Nothing -> Bytes.readFile fileOrUrl
    Just request -> do
      manager <- Client.newTlsManager
      response <- Client.httpLbs request manager
      pure (LazyBytes.toStrict (Client.responseBody response))

putOutput :: Config.Config -> Bytes.ByteString -> IO ()
putOutput = maybe Bytes.putStr Bytes.writeFile . Config.output

getConfig :: [String] -> IO Config.Config
getConfig arguments = do
  let
    (flags, unexpectedArguments, unknownOptions, problems) =
      Console.getOpt' Console.Permute Option.all arguments
  Monad.forM_ unexpectedArguments $ \ x ->
    IO.hPutStrLn IO.stderr $ "WARNING: unexpected argument `" <> x <> "'"
  Monad.forM_ unknownOptions $ \ x ->
    IO.hPutStrLn IO.stderr $ "WARNING: unknown option `" <> x <> "'"
  Monad.forM_ problems $ \ x ->
    IO.hPutStr IO.stderr $ "ERROR: " <> x
  Monad.unless (null problems) Exit.exitFailure
  either fail pure $ Monad.foldM Config.applyFlag Config.initial flags

version :: String
version = Version.showVersion This.version
