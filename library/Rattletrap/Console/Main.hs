module Rattletrap.Console.Main
  ( main
  , rattletrap
  ) where

import qualified Control.Monad as Monad
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Paths_rattletrap as This
import qualified Rattletrap
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as Path
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
  Monad.when (configHelp config) (printHelp name *> Exit.exitFailure)
  Monad.when (configVersion config) (printVersion *> Exit.exitFailure)
  input <- getInput config
  let decode = getDecoder config
  replay <- either fail pure (decode input)
  let encode = getEncoder config
  putOutput config (encode replay)

getDecoder :: Config -> LazyBytes.ByteString -> Either String Rattletrap.Replay
getDecoder config = case getMode config of
  ModeDecode -> Rattletrap.decodeReplayFile
  ModeEncode -> Rattletrap.decodeReplayJson

getEncoder :: Config -> Rattletrap.Replay -> LazyBytes.ByteString
getEncoder config = case getMode config of
  ModeDecode ->
    if configCompact config then Json.encode else Rattletrap.encodeReplayJson
  ModeEncode -> Rattletrap.encodeReplayFile

getInput :: Config -> IO LazyBytes.ByteString
getInput config = case configInput config of
  Nothing -> LazyBytes.getContents
  Just fileOrUrl -> case Client.parseUrlThrow fileOrUrl of
    Nothing -> LazyBytes.readFile fileOrUrl
    Just request -> do
      manager <- Client.newTlsManager
      response <- Client.httpLbs request manager
      pure (Client.responseBody response)

putOutput :: Config -> LazyBytes.ByteString -> IO ()
putOutput config = case configOutput config of
  Nothing -> LazyBytes.putStr
  Just file -> LazyBytes.writeFile file

getConfig :: [String] -> IO Config
getConfig arguments = do
  let
    (updates, unexpectedArguments, unknownOptions, problems) =
      Console.getOpt' Console.Permute options arguments
  printUnexpectedArguments unexpectedArguments
  printUnknownOptions unknownOptions
  printProblems problems
  Monad.unless (null problems) Exit.exitFailure
  either fail pure (Monad.foldM applyUpdate defaultConfig updates)

type Option = Console.OptDescr Update

type Update = Config -> Either String Config

options :: [Option]
options =
  [ compactOption
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
  (Console.NoArg (\config -> pure config { configCompact = True }))
  "minify JSON output"

helpOption :: Option
helpOption = Console.Option
  ['h']
  ["help"]
  (Console.NoArg (\config -> pure config { configHelp = True }))
  "show the help"

inputOption :: Option
inputOption = Console.Option
  ['i']
  ["input"]
  ( Console.ReqArg
    (\input config -> pure config { configInput = Just input })
    "FILE|URL"
  )
  "input file or URL"

modeOption :: Option
modeOption = Console.Option
  ['m']
  ["mode"]
  ( Console.ReqArg
    ( \rawMode config -> do
      mode <- parseMode rawMode
      pure config { configMode = Just mode }
    )
    "MODE"
  )
  "decode or encode"

outputOption :: Option
outputOption = Console.Option
  ['o']
  ["output"]
  ( Console.ReqArg
    (\output config -> pure config { configOutput = Just output })
    "FILE"
  )
  "output file"

versionOption :: Option
versionOption = Console.Option
  ['v']
  ["version"]
  (Console.NoArg (\config -> pure config { configVersion = True }))
  "show the version"

applyUpdate :: Config -> Update -> Either String Config
applyUpdate config update = update config

data Config = Config
  { configCompact :: Bool
  , configHelp :: Bool
  , configInput :: Maybe String
  , configMode :: Maybe Mode
  , configOutput :: Maybe String
  , configVersion :: Bool
  } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { configCompact = False
  , configHelp = False
  , configInput = Nothing
  , configMode = Nothing
  , configOutput = Nothing
  , configVersion = False
  }

getMode :: Config -> Mode
getMode config = case getExtension (configInput config) of
  ".json" -> ModeEncode
  ".replay" -> ModeDecode
  _ -> case getExtension (configOutput config) of
    ".json" -> ModeDecode
    ".replay" -> ModeEncode
    _ -> ModeDecode

getExtension :: Maybe String -> String
getExtension = maybe "" Path.takeExtension

data Mode
  = ModeDecode
  | ModeEncode
  deriving (Show)

parseMode :: String -> Either String Mode
parseMode mode = case mode of
  "decode" -> pure ModeDecode
  "encode" -> pure ModeEncode
  _ -> fail (Printf.printf "invalid mode: %s" (show mode))

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
