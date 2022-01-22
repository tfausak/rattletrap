{- hlint ignore "Avoid restricted extensions" -}
{-# LANGUAGE TypeApplications #-}

module Rattletrap.Console.Main where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Rattletrap.Console.Config as Config
import qualified Rattletrap.Console.Mode as Mode
import qualified Rattletrap.Console.Option as Option
import qualified Rattletrap.Type.Replay as Replay
import qualified Rattletrap.Utility.Helper as Rattletrap
import qualified Rattletrap.Vendor.Argo as Argo
import qualified Rattletrap.Version as Version
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
  if Config.help config
    then helpMain name
    else if Config.version config
      then versionMain
      else if Config.schema config
        then schemaMain config
        else defaultMain config

helpMain :: String -> IO ()
helpMain name = do
  putStr $ Console.usageInfo
    (unwords [name, "version", Version.string])
    Option.all

versionMain :: IO ()
versionMain = do
  putStrLn Version.string

schemaMain :: Config.Config -> IO ()
schemaMain config =
  putOutput config . encodeJson config $ Argo.schema (Argo.codec @Argo.Value) -- TODO: https://github.com/tfausak/argo/issues/46

defaultMain :: Config.Config -> IO ()
defaultMain config = do
  input <- getInput config
  let decode = getDecoder config
  replay <- case decode input of
    Left (ls, e) -> do
      IO.hPutStr IO.stderr $ unlines
        [ "ERROR: " <> Exception.displayException e
        , "-- Context: " <> List.intercalate ", " ls
        , "-- You are using Rattletrap version " <> Version.string
        , "-- " <> show config
        , "-- Please report this problem at https://github.com/tfausak/rattletrap/issues/new"
        ]
      Exit.exitFailure
    Right x -> pure x
  let encode = getEncoder config
  putOutput config (encode replay)

getDecoder
  :: Config.Config
  -> ByteString.ByteString
  -> Either ([String], Exception.SomeException) Replay.Replay
getDecoder config = case Config.getMode config of
  Mode.Decode ->
    Rattletrap.decodeReplayFile (Config.fast config) (Config.skipCrc config)
  Mode.Encode -> Rattletrap.decodeReplayJson

getEncoder :: Config.Config -> Replay.Replay -> LazyByteString.ByteString
getEncoder config = case Config.getMode config of
  Mode.Decode -> encodeJson config
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

putOutput :: Config.Config -> LazyByteString.ByteString -> IO ()
putOutput =
  maybe LazyByteString.putStr LazyByteString.writeFile . Config.output

encodeJson
  :: Argo.HasCodec a => Config.Config -> a -> LazyByteString.ByteString
encodeJson config = Builder.toLazyByteString
  . if Config.compact config then Argo.encode else Argo.encodeWith Argo.Tab

getConfig :: [String] -> IO Config.Config
getConfig arguments = do
  let
    (flags, unexpectedArguments, unknownOptions, problems) =
      Console.getOpt' Console.Permute Option.all arguments
  Monad.forM_ unexpectedArguments $ \x ->
    IO.hPutStrLn IO.stderr $ "WARNING: unexpected argument `" <> x <> "'"
  Monad.forM_ unknownOptions
    $ \x -> IO.hPutStrLn IO.stderr $ "WARNING: unknown option `" <> x <> "'"
  Monad.forM_ problems $ \x -> IO.hPutStr IO.stderr $ "ERROR: " <> x
  Monad.unless (null problems) Exit.exitFailure
  either fail pure $ Monad.foldM Config.applyFlag Config.initial flags
