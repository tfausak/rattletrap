{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main
  ) where

import Rattletrap

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as Casing
import qualified Data.Aeson.TH as Aeson
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Version as Version
import qualified Language.Haskell.TH as TH
import qualified System.Environment as Environment

main :: IO ()
main = do
  args <- Environment.getArgs
  mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs args =
  case args of
    ["version"] -> putStrLn (Version.showVersion version)
    "decode":files -> do
      (getInput, putOutput) <- getIO files
      input <- getInput
      let replay = Binary.runGet getReplay input
      let output = Aeson.encode replay
      putOutput output
    "encode":files -> do
      (getInput, putOutput) <- getIO files
      input <- getInput
      case Aeson.eitherDecode input of
        Left message -> fail ("could not parse JSON: " ++ message)
        Right replay -> do
          let output = Binary.runPut (putReplay replay)
          putOutput output
    _ -> fail ("unexpected arguments " ++ show args)

getIO
  :: Monad m
  => [FilePath] -> m (IO ByteString.ByteString, ByteString.ByteString -> IO ())
getIO files =
  case files of
    [] -> pure (ByteString.getContents, ByteString.putStr)
    [i] -> pure (ByteString.readFile i, ByteString.putStr)
    [i, o] -> pure (ByteString.readFile i, ByteString.writeFile o)
    _ -> fail ("unexpected arguments " ++ show files)

$(Monad.foldM
    (\declarations name -> do
       let options =
             Casing.aesonDrop (length (TH.nameBase name)) Casing.snakeCase
       newDeclarations <- Aeson.deriveJSON options name
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
    , ''FlaggedIntAttributeValue
    , ''Float32
    , ''FloatAttributeValue
    , ''Frame
    , ''GameModeAttributeValue
    , ''Header
    , ''Initialization
    , ''Int32
    , ''Int8
    , ''Int8Vector
    , ''IntAttributeValue
    , ''KeyFrame
    , ''List
    , ''LoadoutAttributeValue
    , ''LoadoutOnlineAttributeValue
    , ''LoadoutsAttributeValue
    , ''LoadoutsOnlineAttributeValue
    , ''LocationAttributeValue
    , ''Mark
    , ''Message
    , ''MusicStingerAttributeValue
    , ''PartyLeaderAttributeValue
    , ''PickupAttributeValue
    , ''PrivateMatchSettingsAttributeValue
    , ''Property
    , ''PropertyValue
    , ''QWordAttributeValue
    , ''RemoteId
    , ''Replay
    , ''Replication
    , ''ReplicationValue
    , ''ReservationAttributeValue
    , ''RigidBodyStateAttributeValue
    , ''SpawnedReplicationValue
    , ''StringAttributeValue
    , ''TeamPaintAttributeValue
    , ''Text
    , ''UniqueIdAttributeValue
    , ''UpdatedReplicationValue
    , ''Vector
    , ''WeldedInfoAttributeValue
    , ''Word32
    , ''Word64
    , ''Word8
    ])
