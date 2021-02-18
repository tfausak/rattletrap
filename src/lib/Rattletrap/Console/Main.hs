module Rattletrap.Console.Main
  ( main
  , rattletrap
  ) where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Paths_rattletrap as Package
import qualified Rattletrap.Console.Config as Config
import qualified Rattletrap.Console.Mode as Mode
import qualified Rattletrap.Console.Option as Option
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Content as Content
import qualified Rattletrap.Type.Header as Header
import qualified Rattletrap.Type.Replay as Replay
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Helper as Rattletrap
import qualified Rattletrap.Utility.Json as Json
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
  Monad.when (Config.schema config) $ do
    LazyByteString.putStr $ Aeson.encodePretty' Aeson.defConfig
      { Aeson.confCompare = compare
      , Aeson.confIndent = Aeson.Tab
      , Aeson.confTrailingNewline = True
      } schema
    Exit.exitSuccess
  input <- getInput config
  let decode = getDecoder config
  replay <- either fail pure (decode input)
  let encode = getEncoder config
  putOutput config (encode replay)

schema :: Aeson.Value
schema = Aeson.object
  [ Json.pair "$schema" "https://json-schema.org/draft-07/schema"
  , Json.pair "$ref" "#/definitions/replay"
  , Json.pair "definitions" . Aeson.object $ fmap
    (\ s -> Schema.name s Aeson..= Schema.json s)
    [ Content.schema
    , Header.schema
    , Replay.schema
    , U32.schema
    ]
  ]

getDecoder :: Config.Config -> ByteString.ByteString -> Either String Replay.Replay
getDecoder config = case Config.getMode config of
  Mode.Decode -> Rattletrap.decodeReplayFile (Config.fast config) (Config.skipCrc config)
  Mode.Encode -> Rattletrap.decodeReplayJson

getEncoder :: Config.Config -> Replay.Replay -> ByteString.ByteString
getEncoder config = case Config.getMode config of
  Mode.Decode -> if Config.compact config
    then LazyByteString.toStrict . Aeson.encode
    else Rattletrap.encodeReplayJson
  Mode.Encode -> Rattletrap.encodeReplayFile $ Config.fast config

getInput :: Config.Config -> IO ByteString.ByteString
getInput config = case Config.input config of
  Nothing -> ByteString.getContents
  Just fileOrUrl -> case Client.parseUrlThrow fileOrUrl of
    Nothing -> ByteString.readFile fileOrUrl
    Just request -> do
      manager <- Client.newTlsManager
      response <- Client.httpLbs request manager
      pure (LazyByteString.toStrict (Client.responseBody response))

putOutput :: Config.Config -> ByteString.ByteString -> IO ()
putOutput = maybe ByteString.putStr ByteString.writeFile . Config.output

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
version = Version.showVersion Package.version
