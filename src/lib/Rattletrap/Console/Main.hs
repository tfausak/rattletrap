module Rattletrap.Console.Main where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Bool as Bool
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Rattletrap.Console.Config as Config
import qualified Rattletrap.Console.Mode as Mode
import qualified Rattletrap.Console.Option as Option
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute as Attribute
import qualified Rattletrap.Type.Attribute.AppliedDamage as Attribute.AppliedDamage
import qualified Rattletrap.Type.Attribute.Boolean as Attribute.Boolean
import qualified Rattletrap.Type.Attribute.Byte as Attribute.Byte
import qualified Rattletrap.Type.Attribute.CamSettings as Attribute.CamSettings
import qualified Rattletrap.Type.Attribute.ClubColors as Attribute.ClubColors
import qualified Rattletrap.Type.Attribute.CustomDemolish as Attribute.CustomDemolish
import qualified Rattletrap.Type.Attribute.DamageState as Attribute.DamageState
import qualified Rattletrap.Type.Attribute.Demolish as Attribute.Demolish
import qualified Rattletrap.Type.Attribute.Enum as Attribute.Enum
import qualified Rattletrap.Type.Attribute.Explosion as Attribute.Explosion
import qualified Rattletrap.Type.Attribute.ExtendedExplosion as Attribute.ExtendedExplosion
import qualified Rattletrap.Type.Attribute.FlaggedByte as Attribute.FlaggedByte
import qualified Rattletrap.Type.Attribute.FlaggedInt as Attribute.FlaggedInt
import qualified Rattletrap.Type.Attribute.Float as Attribute.Float
import qualified Rattletrap.Type.Attribute.GameMode as Attribute.GameMode
import qualified Rattletrap.Type.Attribute.Int as Attribute.Int
import qualified Rattletrap.Type.Attribute.Int64 as Attribute.Int64
import qualified Rattletrap.Type.Attribute.Loadout as Attribute.Loadout
import qualified Rattletrap.Type.Attribute.LoadoutOnline as Attribute.LoadoutOnline
import qualified Rattletrap.Type.Attribute.Loadouts as Attribute.Loadouts
import qualified Rattletrap.Type.Attribute.LoadoutsOnline as Attribute.LoadoutsOnline
import qualified Rattletrap.Type.Attribute.Location as Attribute.Location
import qualified Rattletrap.Type.Attribute.MusicStinger as Attribute.MusicStinger
import qualified Rattletrap.Type.Attribute.PartyLeader as Attribute.PartyLeader
import qualified Rattletrap.Type.Attribute.Pickup as Attribute.Pickup
import qualified Rattletrap.Type.Attribute.PickupInfo as Attribute.PickupInfo
import qualified Rattletrap.Type.Attribute.PickupNew as Attribute.PickupNew
import qualified Rattletrap.Type.Attribute.PlayerHistoryKey as Attribute.PlayerHistoryKey
import qualified Rattletrap.Type.Attribute.PrivateMatchSettings as Attribute.PrivateMatchSettings
import qualified Rattletrap.Type.Attribute.Product as Attribute.Product
import qualified Rattletrap.Type.Attribute.ProductValue as Attribute.ProductValue
import qualified Rattletrap.Type.Attribute.QWord as Attribute.QWord
import qualified Rattletrap.Type.Attribute.RepStatTitle as Attribute.RepStatTitle
import qualified Rattletrap.Type.Attribute.Reservation as Attribute.Reservation
import qualified Rattletrap.Type.Attribute.RigidBodyState as Attribute.RigidBodyState
import qualified Rattletrap.Type.Attribute.Rotation as Attribute.Rotation
import qualified Rattletrap.Type.Attribute.StatEvent as Attribute.StatEvent
import qualified Rattletrap.Type.Attribute.String as Attribute.String
import qualified Rattletrap.Type.Attribute.TeamPaint as Attribute.TeamPaint
import qualified Rattletrap.Type.Attribute.Title as Attribute.Title
import qualified Rattletrap.Type.Attribute.UniqueId as Attribute.UniqueId
import qualified Rattletrap.Type.Attribute.WeldedInfo as Attribute.WeldedInfo
import qualified Rattletrap.Type.AttributeMapping as AttributeMapping
import qualified Rattletrap.Type.AttributeValue as AttributeValue
import qualified Rattletrap.Type.Cache as Cache
import qualified Rattletrap.Type.ClassMapping as ClassMapping
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.CompressedWordVector as CompressedWordVector
import qualified Rattletrap.Type.Content as Content
import qualified Rattletrap.Type.Dictionary as Dictionary
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.Frame as Frame
import qualified Rattletrap.Type.Header as Header
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.I64 as I64
import qualified Rattletrap.Type.I8 as I8
import qualified Rattletrap.Type.Initialization as Initialization
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Keyframe as Keyframe
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Mark as Mark
import qualified Rattletrap.Type.Message as Message
import qualified Rattletrap.Type.Property as Property
import qualified Rattletrap.Type.Property.Array as Property.Array
import qualified Rattletrap.Type.Property.Byte as Property.Byte
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Quaternion as Quaternion
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.RemoteId.PlayStation as RemoteId.PlayStation
import qualified Rattletrap.Type.RemoteId.PsyNet as RemoteId.PsyNet
import qualified Rattletrap.Type.RemoteId.Switch as RemoteId.Switch
import qualified Rattletrap.Type.Replay as Replay
import qualified Rattletrap.Type.Replication as Replication
import qualified Rattletrap.Type.Replication.Destroyed as Replication.Destroyed
import qualified Rattletrap.Type.Replication.Spawned as Replication.Spawned
import qualified Rattletrap.Type.Replication.Updated as Replication.Updated
import qualified Rattletrap.Type.ReplicationValue as ReplicationValue
import qualified Rattletrap.Type.Rotation as Rotation
import qualified Rattletrap.Type.Section as Section
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Utility.Helper as Rattletrap
import qualified Rattletrap.Utility.Json as Json
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
schemaMain config = putOutput config $ encodeJson config schema

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

schema :: Json.Value
schema =
  let contentSchema = Content.schema $ List.schema Frame.schema
  in
    Json.object
      [ Json.pair "$schema" "http://json-schema.org/draft-07/schema"
      , Json.pair "$id" Replay.schemaUrl
      , Json.pair "$ref" "#/definitions/replay"
      , Json.pair "definitions" . Json.object $ fmap
        (\s -> Json.pair (Text.unpack $ Schema.name s) $ Schema.json s)
        [ Attribute.schema
        , Attribute.AppliedDamage.schema
        , Attribute.Boolean.schema
        , Attribute.Byte.schema
        , Attribute.CamSettings.schema
        , Attribute.ClubColors.schema
        , Attribute.CustomDemolish.schema
        , Attribute.DamageState.schema
        , Attribute.Demolish.schema
        , Attribute.Enum.schema
        , Attribute.Explosion.schema
        , Attribute.ExtendedExplosion.schema
        , Attribute.FlaggedByte.schema
        , Attribute.FlaggedInt.schema
        , Attribute.Float.schema
        , Attribute.GameMode.schema
        , Attribute.Int.schema
        , Attribute.Int64.schema
        , Attribute.Loadout.schema
        , Attribute.LoadoutOnline.schema
        , Attribute.Loadouts.schema
        , Attribute.LoadoutsOnline.schema
        , Attribute.Location.schema
        , Attribute.MusicStinger.schema
        , Attribute.PartyLeader.schema
        , Attribute.Pickup.schema
        , Attribute.PickupInfo.schema
        , Attribute.PickupNew.schema
        , Attribute.PlayerHistoryKey.schema
        , Attribute.PrivateMatchSettings.schema
        , Attribute.Product.schema
        , Attribute.ProductValue.schema
        , Attribute.QWord.schema
        , Attribute.RepStatTitle.schema
        , Attribute.Reservation.schema
        , Attribute.RigidBodyState.schema
        , Attribute.Rotation.schema
        , Attribute.StatEvent.schema
        , Attribute.String.schema
        , Attribute.TeamPaint.schema
        , Attribute.Title.schema
        , Attribute.UniqueId.schema
        , Attribute.WeldedInfo.schema
        , AttributeMapping.schema
        , AttributeValue.schema
        , Cache.schema
        , ClassMapping.schema
        , CompressedWord.schema
        , CompressedWordVector.schema
        , contentSchema
        , Dictionary.schema Property.schema
        , F32.schema
        , Frame.schema
        , Header.schema
        , I32.schema
        , I64.schema
        , I8.schema
        , Initialization.schema
        , Int8Vector.schema
        , Keyframe.schema
        , List.schema Attribute.Product.schema
        , Mark.schema
        , Message.schema
        , Property.schema
        , Property.Array.schema Property.schema
        , Property.Byte.schema
        , PropertyValue.schema Property.schema
        , Quaternion.schema
        , RemoteId.schema
        , RemoteId.PlayStation.schema
        , RemoteId.PsyNet.schema
        , RemoteId.Switch.schema
        , Replay.schema (Section.schema Header.schema)
        . Section.schema
        $ contentSchema
        , Replication.Destroyed.schema
        , Replication.schema
        , Replication.Spawned.schema
        , Replication.Updated.schema
        , ReplicationValue.schema
        , Rotation.schema
        , Schema.boolean
        , Schema.integer
        , Schema.null
        , Schema.number
        , Schema.string
        , Section.schema contentSchema
        , Section.schema Header.schema
        , Str.schema
        , U32.schema
        , U64.schema
        , U8.schema
        , Vector.schema
        ]
      ]

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

encodeJson :: Json.ToJSON a => Config.Config -> a -> LazyByteString.ByteString
encodeJson = Bool.bool Json.encodePretty Json.encode . Config.compact

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
