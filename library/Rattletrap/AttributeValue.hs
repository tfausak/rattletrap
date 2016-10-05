module Rattletrap.AttributeValue where

import Rattletrap.Int32
import Rattletrap.Location
import Rattletrap.RemoteId
import Rattletrap.Spin
import Rattletrap.Text
import Rattletrap.Word32
import Rattletrap.Word64
import Rattletrap.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data AttributeValue
  = BooleanAttribute Bool
  | ByteAttribute Word8
  | CamSettingsAttribute
  | DemolishAttribute
  | EnumAttribute
  | ExplosionAttribute
  | FlaggedIntAttribute Bool
                        Int32
  | FloatAttribute
  | GameModeAttribute
  | IntAttribute Int32
  | LoadoutAttribute Word8
                     Word32
                     Word32
                     Word32
                     Word32
                     Word32
                     Word32
                     Word32
                     (Maybe Word32)
  | LoadoutOnlineAttribute
  | LoadoutsAttribute
  | LoadoutsOnlineAttribute
  | LocationAttribute
  | MusicStingerAttribute
  | PickupAttribute
  | PrivateMatchSettingsAttribute
  | QWordAttribute Word64
  | RelativeRotationAttribute
  | ReservationAttribute
  | RigidBodyStateAttribute Bool
                            Location
                            Spin
                            (Maybe Location)
                            (Maybe Location)
  | StringAttribute Text
  | TeamPaintAttribute
  | UniqueIdAttribute Word8
                      RemoteId
                      Word8
  | WeldedInfoAttribute
  deriving (Eq, Ord, Show)

getAttributeValue :: Text -> BinaryBit.BitGet AttributeValue
getAttributeValue name =
  case textToString name of
    "Engine.Actor:bBlockActors" -> getBooleanAttribute
    "Engine.GameReplicationInfo:GameClass" -> getFlaggedIntAttribute
    "Engine.GameReplicationInfo:ServerName" -> getStringAttribute
    "Engine.PlayerReplicationInfo:bReadyToPlay" -> getBooleanAttribute
    "Engine.PlayerReplicationInfo:Ping" -> getByteAttribute
    "Engine.PlayerReplicationInfo:PlayerID" -> getIntAttribute
    "Engine.PlayerReplicationInfo:PlayerName" -> getStringAttribute
    "Engine.PlayerReplicationInfo:Team" -> getFlaggedIntAttribute
    "Engine.PlayerReplicationInfo:UniqueId" -> getUniqueIdAttribute
    "ProjectX.GRI_X:bGameStarted" -> getBooleanAttribute
    "ProjectX.GRI_X:GameServerID" -> getQWordAttribute
    "TAGame.Ball_TA:GameEvent" -> getFlaggedIntAttribute
    "TAGame.PRI_TA:bOnlineLoadoutSet" -> getBooleanAttribute
    "TAGame.PRI_TA:ClientLoadout" -> getLoadoutAttribute
    "TAGame.PRI_TA:PersistentCamera" -> getFlaggedIntAttribute
    "TAGame.PRI_TA:ReplicatedGameEvent" -> getFlaggedIntAttribute
    "TAGame.PRI_TA:Title" -> getIntAttribute
    "TAGame.PRI_TA:TotalXP" -> getIntAttribute
    "TAGame.RBActor_TA:ReplicatedRBState" -> getRigidBodyStateAttribute
    "TAGame.Team_TA:GameEvent" -> getFlaggedIntAttribute
    _ -> fail ("don't know how to get attribute value " ++ show name)

getBooleanAttribute :: BinaryBit.BitGet AttributeValue
getBooleanAttribute = do
  x <- BinaryBit.getBool
  pure (BooleanAttribute x)

getByteAttribute :: BinaryBit.BitGet AttributeValue
getByteAttribute = do
  byte <- getWord8Bits
  pure (ByteAttribute byte)

getFlaggedIntAttribute :: BinaryBit.BitGet AttributeValue
getFlaggedIntAttribute = do
  flag <- BinaryBit.getBool
  int <- getInt32Bits
  pure (FlaggedIntAttribute flag int)

getIntAttribute :: BinaryBit.BitGet AttributeValue
getIntAttribute = do
  int <- getInt32Bits
  pure (IntAttribute int)

getLoadoutAttribute :: BinaryBit.BitGet AttributeValue
getLoadoutAttribute = do
  version <- getWord8Bits
  body <- getWord32Bits
  decal <- getWord32Bits
  wheels <- getWord32Bits
  rocketTrail <- getWord32Bits
  antenna <- getWord32Bits
  topper <- getWord32Bits
  g <- getWord32Bits
  h <-
    if version > Word8 10
      then do
        h <- getWord32Bits
        pure (Just h)
      else pure Nothing
  pure
    (LoadoutAttribute version body decal wheels rocketTrail antenna topper g h)

getQWordAttribute :: BinaryBit.BitGet AttributeValue
getQWordAttribute = do
  word64 <- getWord64Bits
  pure (QWordAttribute word64)

getRigidBodyStateAttribute :: BinaryBit.BitGet AttributeValue
getRigidBodyStateAttribute = do
  isSleeping <- BinaryBit.getBool
  location <- getLocation
  spin <- getSpin
  linearVelocity <-
    if isSleeping
      then pure Nothing
      else do
        linearVelocity <- getLocation
        pure (Just linearVelocity)
  angularVelocity <-
    if isSleeping
      then pure Nothing
      else do
        angularVelocity <- getLocation
        pure (Just angularVelocity)
  pure
    (RigidBodyStateAttribute
       isSleeping
       location
       spin
       linearVelocity
       angularVelocity)

getStringAttribute :: BinaryBit.BitGet AttributeValue
getStringAttribute = do
  text <- getTextBits
  pure (StringAttribute text)

getUniqueIdAttribute :: BinaryBit.BitGet AttributeValue
getUniqueIdAttribute = do
  systemId <- getWord8Bits
  remoteId <- getRemoteId systemId
  localId <- getWord8Bits
  pure (UniqueIdAttribute systemId remoteId localId)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
    BooleanAttribute x -> BinaryBit.putBool x
    ByteAttribute byte -> putWord8Bits byte
    FlaggedIntAttribute flag int -> do
      BinaryBit.putBool flag
      putInt32Bits int
    IntAttribute int -> putInt32Bits int
    LoadoutAttribute version body decal wheels rocketTrail antenna topper g h -> do
      putWord8Bits version
      putWord32Bits body
      putWord32Bits decal
      putWord32Bits wheels
      putWord32Bits rocketTrail
      putWord32Bits antenna
      putWord32Bits topper
      putWord32Bits g
      case h of
        Nothing -> pure ()
        Just x -> putWord32Bits x
    QWordAttribute word64 -> putWord64Bits word64
    RigidBodyStateAttribute isSleeping location spin maybeLinearVelocity maybeAngularVelocity -> do
      BinaryBit.putBool isSleeping
      putLocation location
      putSpin spin
      case maybeLinearVelocity of
        Nothing -> pure ()
        Just linearVelocity -> putLocation linearVelocity
      case maybeAngularVelocity of
        Nothing -> pure ()
        Just angularVelocity -> putLocation angularVelocity
    StringAttribute text -> putTextBits text
    UniqueIdAttribute systemId remoteId localId -> do
      putWord8Bits systemId
      putRemoteId remoteId
      putWord8Bits localId
    _ -> fail ("don't know how to put attribute value " ++ show value)
