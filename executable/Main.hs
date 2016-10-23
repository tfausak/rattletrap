{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main
  ) where

import Rattletrap

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
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
    ["parse", replayFile, jsonFile] -> do
      input <- ByteString.readFile replayFile
      let replay = Binary.runGet getReplay input
      let output = Aeson.encode replay
      ByteString.writeFile jsonFile output
    ["generate", jsonFile, replayfile] -> do
      input <- ByteString.readFile jsonFile
      case Aeson.eitherDecode input of
        Left message -> fail ("could not parse JSON: " ++ message)
        Right replay -> do
          let output = Binary.runPut (putReplay replay)
          ByteString.writeFile replayfile output
    _ -> fail ("unexpected arguments " ++ show args)

$(Monad.foldM
    (\declarations name -> do
       newDeclarations <- Aeson.deriveJSON Aeson.defaultOptions name
       pure (newDeclarations ++ declarations))
    []
    [ ''Attribute
    , ''AttributeMapping
    , ''AttributeValue
    , ''BooleanAttributeValue
    , ''ByteAttributeValue
    , ''Cache
    , ''CamSettingsAttributeValue
    , ''ClassMapping
    , ''CompressedWord
    , ''CompressedWordVector
    , ''Content
    , ''DemolishAttributeValue
    , ''DestroyedReplicationValue
    , ''Dictionary
    , ''EnumAttributeValue
    , ''ExplosionAttributeValue
    , ''Float32
    , ''FloatAttributeValue
    , ''Frame
    , ''Header
    , ''Initialization
    , ''Int32
    , ''Int8
    , ''Int8Vector
    , ''IntAttributeValue
    , ''KeyFrame
    , ''List
    , ''LocationAttributeValue
    , ''Mark
    , ''Message
    , ''Property
    , ''PropertyValue
    , ''QWordAttributeValue
    , ''RemoteId
    , ''Replay
    , ''Replication
    , ''ReplicationValue
    , ''SpawnedReplicationValue
    , ''StringAttributeValue
    , ''Text
    , ''UpdatedReplicationValue
    , ''Vector
    , ''Word32
    , ''Word64
    , ''Word8
    ])
