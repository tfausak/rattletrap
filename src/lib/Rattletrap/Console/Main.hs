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
import qualified Rattletrap.Type.Replay as Replay
import qualified Rattletrap.Utility.Helper as Rattletrap
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Printf as Printf

main :: IO ()
main = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  rattletrap name arguments

rattletrap :: String -> [String] -> IO ()
rattletrap name arguments = do
  config <- getConfig arguments
  Monad.when (Config.help config) (printHelp name *> Exit.exitFailure)
  Monad.when (Config.version config) (printVersion *> Exit.exitFailure)
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
    (updates, unexpectedArguments, unknownOptions, problems) =
      Console.getOpt' Console.Permute options arguments
  printUnexpectedArguments unexpectedArguments
  printUnknownOptions unknownOptions
  printProblems problems
  Monad.unless (null problems) Exit.exitFailure
  either fail pure (Monad.foldM applyUpdate Config.initial updates)

type Option = Console.OptDescr Update

type Update = Config.Config -> Either String Config.Config

options :: [Option]
options =
  [ compactOption
  , fastOption
  , helpOption
  , inputOption
  , modeOption
  , outputOption
  , versionOption
  ]

compactOption :: Option
compactOption = Console.Option
  ['c']
  ["compact"]
  (Console.NoArg (\config -> pure config { Config.compact = True }))
  "minify JSON output"

fastOption :: Option
fastOption = Console.Option
  ['f']
  ["fast"]
  (Console.NoArg (\config -> pure config { Config.fast = True }))
  "only encode or decode the header"

helpOption :: Option
helpOption = Console.Option
  ['h']
  ["help"]
  (Console.NoArg (\config -> pure config { Config.help = True }))
  "show the help"

inputOption :: Option
inputOption = Console.Option
  ['i']
  ["input"]
  (Console.ReqArg
    (\input config -> pure config { Config.input = Just input })
    "FILE|URL"
  )
  "input file or URL"

modeOption :: Option
modeOption = Console.Option
  ['m']
  ["mode"]
  (Console.ReqArg
    (\rawMode config -> do
      mode <- Mode.fromString rawMode
      pure config { Config.mode = Just mode }
    )
    "MODE"
  )
  "decode or encode"

outputOption :: Option
outputOption = Console.Option
  ['o']
  ["output"]
  (Console.ReqArg
    (\output config -> pure config { Config.output = Just output })
    "FILE"
  )
  "output file"

versionOption :: Option
versionOption = Console.Option
  ['v']
  ["version"]
  (Console.NoArg (\config -> pure config { Config.version = True }))
  "show the version"

applyUpdate :: Config.Config -> Update -> Either String Config.Config
applyUpdate config update = update config

printUnexpectedArguments :: [String] -> IO ()
printUnexpectedArguments = mapM_ printUnexpectedArgument

printUnexpectedArgument :: String -> IO ()
printUnexpectedArgument =
  warnLn . Printf.printf "WARNING: unexpected argument `%s'"

printUnknownOptions :: [String] -> IO ()
printUnknownOptions = mapM_ printUnknownOption

printUnknownOption :: String -> IO ()
printUnknownOption = warnLn . Printf.printf "WARNING: unknown option `%s'"

printProblems :: [String] -> IO ()
printProblems = mapM_ printProblem

printProblem :: String -> IO ()
printProblem = warn . Printf.printf "ERROR: %s"

printHelp :: String -> IO ()
printHelp = warn . help

help :: String -> String
help name = Console.usageInfo (header name) options

header :: String -> String
header name = unwords [name, "version", version]

version :: String
version = Version.showVersion This.version

printVersion :: IO ()
printVersion = warnLn version

warn :: String -> IO ()
warn = IO.hPutStr IO.stderr

warnLn :: String -> IO ()
warnLn = IO.hPutStrLn IO.stderr
