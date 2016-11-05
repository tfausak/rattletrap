#!/usr/bin/env stack
{- stack runghc
  --package aeson
  --package bytestring
  --package directory
  --package filepath
  --package process
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Maybe as Maybe
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  maybeTag <- Environment.lookupEnv "TRAVIS_TAG"
  Monad.when (Maybe.isNothing maybeTag) (do
    putStrLn "The $TRAVIS_TAG variable is empty."
    Exit.exitSuccess)

  maybeOs <- Environment.lookupEnv "TRAVIS_OS_NAME"
  Monad.unless (maybeOs == Just "linux") (do
    putStrLn "The $TRAVIS_OS_NAME variable is not 'linux'."
    Exit.exitSuccess)

  home <- Directory.getHomeDirectory
  let directory = FilePath.joinPath [home, ".stack", "upload"]
  Directory.createDirectoryIfMissing True directory

  let file = FilePath.joinPath [directory, "credentials.json"]
  username <- Environment.getEnv "HACKAGE_USERNAME"
  password <- Environment.getEnv "HACKAGE_PASSWORD"
  let credentials = Aeson.object
        [ "username" Aeson..= username
        , "password" Aeson..= password
        ]
  ByteString.writeFile file (Aeson.encode credentials)

  Process.callProcess "stack" ["upload", "."]
