module Rattletrap.AttributeValue where

import Rattletrap.CompressedWord
import Rattletrap.Float32
import Rattletrap.Int32
import Rattletrap.Location
import Rattletrap.RemoteId
import Rattletrap.Spin
import Rattletrap.Text
import Rattletrap.Word32
import Rattletrap.Word64
import Rattletrap.Word8

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Word as Word

data AttributeValue
  = BooleanAttribute Bool
  | ByteAttribute Word8
  | CamSettingsAttribute Float32
                         Float32
                         Float32
                         Float32
                         Float32
                         Float32
  | DemolishAttribute
  | EnumAttribute Word.Word16
  | ExplosionAttribute
  | FlaggedIntAttribute Bool
                        Int32
  | FloatAttribute Float32
  | GameModeAttribute Word.Word8
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
  | LoadoutOnlineAttribute [[(Word32, CompressedWord)]]
  | LoadoutsAttribute AttributeValue
                      AttributeValue
  | LoadoutsOnlineAttribute AttributeValue
                            AttributeValue
                            Bool
                            Bool
  | LocationAttribute Location
  | MusicStingerAttribute
  | PartyLeaderAttribute Word8
                         (Maybe (RemoteId, Word8))
  | PickupAttribute Bool
                    (Maybe Word32)
                    Bool
  | PrivateMatchSettingsAttribute
  | QWordAttribute Word64
  | RelativeRotationAttribute
  | ReservationAttribute CompressedWord
                         Word8
                         RemoteId
                         Word8
                         (Maybe Text)
                         Bool
                         Bool
  | RigidBodyStateAttribute Bool
                            Location
                            Spin
                            (Maybe Location)
                            (Maybe Location)
  | StringAttribute Text
  | TeamPaintAttribute Word8
                       Word8
                       Word8
                       Word32
                       Word32
  | UniqueIdAttribute Word8
                      RemoteId
                      Word8
  | WeldedInfoAttribute
  deriving (Eq, Ord, Show)

getAttributeValue :: Text -> BinaryBit.BitGet AttributeValue
getAttributeValue name =
  case textToString name of
    "Engine.Actor:bBlockActors" -> getBooleanAttribute
    "Engine.Actor:bCollideActors" -> getBooleanAttribute
    "Engine.Actor:DrawScale" -> getFloatAttribute
    "Engine.Actor:Role" -> getEnumAttribute
    "Engine.GameReplicationInfo:GameClass" -> getFlaggedIntAttribute
    "Engine.GameReplicationInfo:ServerName" -> getStringAttribute
    "Engine.Pawn:PlayerReplicationInfo" -> getFlaggedIntAttribute
    "Engine.PlayerReplicationInfo:bIsSpectator" -> getBooleanAttribute
    "Engine.PlayerReplicationInfo:bReadyToPlay" -> getBooleanAttribute
    "Engine.PlayerReplicationInfo:Ping" -> getByteAttribute
    "Engine.PlayerReplicationInfo:PlayerID" -> getIntAttribute
    "Engine.PlayerReplicationInfo:PlayerName" -> getStringAttribute
    "Engine.PlayerReplicationInfo:Team" -> getFlaggedIntAttribute
    "Engine.PlayerReplicationInfo:UniqueId" -> getUniqueIdAttribute
    "ProjectX.GRI_X:bGameStarted" -> getBooleanAttribute
    "ProjectX.GRI_X:GameServerID" -> getQWordAttribute
    "ProjectX.GRI_X:ReplicatedGameMutatorIndex" -> getIntAttribute
    "ProjectX.GRI_X:ReplicatedGamePlaylist" -> getIntAttribute
    "ProjectX.GRI_X:Reservations" -> getReservationAttribute
    "TAGame.Ball_TA:GameEvent" -> getFlaggedIntAttribute
    "TAGame.Ball_TA:HitTeamNum" -> getByteAttribute
    "TAGame.CameraSettingsActor_TA:bUsingBehindView" -> getBooleanAttribute
    "TAGame.CameraSettingsActor_TA:bUsingSecondaryCamera" -> getBooleanAttribute
    "TAGame.CameraSettingsActor_TA:CameraYaw" -> getByteAttribute
    "TAGame.CameraSettingsActor_TA:PRI" -> getFlaggedIntAttribute
    "TAGame.CameraSettingsActor_TA:ProfileSettings" -> getCamSettingsAttribute
    "TAGame.Car_TA:TeamPaint" -> getTeamPaintAttribute
    "TAGame.CarComponent_Boost_TA:bUnlimitedBoost" -> getBooleanAttribute
    "TAGame.CarComponent_Boost_TA:RechargeDelay" -> getFloatAttribute
    "TAGame.CarComponent_Boost_TA:RechargeRate" -> getFloatAttribute
    "TAGame.CarComponent_Boost_TA:ReplicatedBoostAmount" -> getByteAttribute
    "TAGame.CarComponent_Boost_TA:UnlimitedBoostRefCount" -> getIntAttribute
    "TAGame.CarComponent_Dodge_TA:DodgeTorque" -> getLocationAttribute
    "TAGame.CarComponent_TA:ReplicatedActive" -> getByteAttribute
    "TAGame.CarComponent_TA:Vehicle" -> getFlaggedIntAttribute
    "TAGame.CrowdActor_TA:GameEvent" -> getFlaggedIntAttribute
    "TAGame.CrowdActor_TA:ModifiedNoise" -> getFloatAttribute
    "TAGame.CrowdActor_TA:ReplicatedOneShotSound" -> getFlaggedIntAttribute
    "TAGame.CrowdManager_TA:GameEvent" -> getFlaggedIntAttribute
    "TAGame.GameEvent_Soccar_TA:bBallHasBeenHit" -> getBooleanAttribute
    "TAGame.GameEvent_Soccar_TA:RoundNum" -> getIntAttribute
    "TAGame.GameEvent_Soccar_TA:SecondsRemaining" -> getIntAttribute
    "TAGame.GameEvent_TA:BotSkill" -> getIntAttribute
    "TAGame.GameEvent_TA:GameMode" -> getGameModeAttribute
    "TAGame.GameEvent_TA:MatchTypeClass" -> getFlaggedIntAttribute
    "TAGame.GameEvent_TA:ReplicatedGameStateTimeRemaining" -> getIntAttribute
    "TAGame.GameEvent_TA:ReplicatedStateName" -> getIntAttribute
    "TAGame.GameEvent_Team_TA:MaxTeamSize" -> getIntAttribute
    "TAGame.PRI_TA:bOnlineLoadoutSet" -> getBooleanAttribute
    "TAGame.PRI_TA:bOnlineLoadoutsSet" -> getBooleanAttribute
    "TAGame.PRI_TA:ClientLoadout" -> getLoadoutAttribute
    "TAGame.PRI_TA:ClientLoadoutOnline" -> getLoadoutOnlineAttribute
    "TAGame.PRI_TA:ClientLoadouts" -> getLoadoutsAttribute
    "TAGame.PRI_TA:ClientLoadoutsOnline" -> getLoadoutsOnlineAttribute
    "TAGame.PRI_TA:MatchScore" -> getIntAttribute
    "TAGame.PRI_TA:MatchShots" -> getIntAttribute
    "TAGame.PRI_TA:PartyLeader" -> getPartyLeaderAttribute
    "TAGame.PRI_TA:PersistentCamera" -> getFlaggedIntAttribute
    "TAGame.PRI_TA:ReplicatedGameEvent" -> getFlaggedIntAttribute
    "TAGame.PRI_TA:Title" -> getIntAttribute
    "TAGame.PRI_TA:TotalXP" -> getIntAttribute
    "TAGame.RBActor_TA:ReplicatedRBState" -> getRigidBodyStateAttribute
    "TAGame.Team_TA:GameEvent" -> getFlaggedIntAttribute
    "TAGame.Vehicle_TA:bDriving" -> getBooleanAttribute
    "TAGame.Vehicle_TA:bReplicatedHandbrake" -> getBooleanAttribute
    "TAGame.Vehicle_TA:ReplicatedSteer" -> getByteAttribute
    "TAGame.Vehicle_TA:ReplicatedThrottle" -> getByteAttribute
    "TAGame.VehiclePickup_TA:ReplicatedPickupData" -> getPickupAttribute
    _ -> fail ("don't know how to get attribute value " ++ show name)

getBooleanAttribute :: BinaryBit.BitGet AttributeValue
getBooleanAttribute = do
  x <- BinaryBit.getBool
  pure (BooleanAttribute x)

getByteAttribute :: BinaryBit.BitGet AttributeValue
getByteAttribute = do
  byte <- getWord8Bits
  pure (ByteAttribute byte)

getCamSettingsAttribute :: BinaryBit.BitGet AttributeValue
getCamSettingsAttribute = do
  fov <- getFloat32Bits
  height <- getFloat32Bits
  angle <- getFloat32Bits
  distance <- getFloat32Bits
  stiffness <- getFloat32Bits
  swivelSpeed <- getFloat32Bits
  pure (CamSettingsAttribute fov height angle distance stiffness swivelSpeed)

getEnumAttribute :: BinaryBit.BitGet AttributeValue
getEnumAttribute = do
  x <- BinaryBit.getWord16be 11
  pure (EnumAttribute x)

getFlaggedIntAttribute :: BinaryBit.BitGet AttributeValue
getFlaggedIntAttribute = do
  flag <- BinaryBit.getBool
  int <- getInt32Bits
  pure (FlaggedIntAttribute flag int)

getFloatAttribute :: BinaryBit.BitGet AttributeValue
getFloatAttribute = do
  float <- getFloat32Bits
  pure (FloatAttribute float)

getGameModeAttribute :: BinaryBit.BitGet AttributeValue
getGameModeAttribute = do
  word8 <- BinaryBit.getWord8 8
  pure (GameModeAttribute word8)

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

getLoadoutOnlineAttribute :: BinaryBit.BitGet AttributeValue
getLoadoutOnlineAttribute = do
  size <- getWord8Bits
  values <-
    Monad.replicateM
      (fromIntegral (word8Value size))
      (do innerSize <- getWord8Bits
          Monad.replicateM
            (fromIntegral (word8Value innerSize))
            (do x <- getWord32Bits
                y <- getCompressedWord 27
                pure (x, y)))
  pure (LoadoutOnlineAttribute values)

getLoadoutsAttribute :: BinaryBit.BitGet AttributeValue
getLoadoutsAttribute = do
  blueLoadout <- getLoadoutAttribute
  orangeLoadout <- getLoadoutAttribute
  pure (LoadoutsAttribute blueLoadout orangeLoadout)

getLoadoutsOnlineAttribute :: BinaryBit.BitGet AttributeValue
getLoadoutsOnlineAttribute = do
  blueLoadout <- getLoadoutOnlineAttribute
  orangeLoadout <- getLoadoutOnlineAttribute
  unknown1 <- BinaryBit.getBool
  unknown2 <- BinaryBit.getBool
  pure (LoadoutsOnlineAttribute blueLoadout orangeLoadout unknown1 unknown2)

getLocationAttribute :: BinaryBit.BitGet AttributeValue
getLocationAttribute = do
  location <- getLocation
  pure (LocationAttribute location)

getPartyLeaderAttribute :: BinaryBit.BitGet AttributeValue
getPartyLeaderAttribute = do
  systemId <- getWord8Bits
  maybeRemoteAndLocalId <-
    if systemId == Word8 0
      then pure Nothing
      else do
        remoteId <- getRemoteId systemId
        localId <- getWord8Bits
        pure (Just (remoteId, localId))
  pure (PartyLeaderAttribute systemId maybeRemoteAndLocalId)

getPickupAttribute :: BinaryBit.BitGet AttributeValue
getPickupAttribute = do
  instigator <- BinaryBit.getBool
  maybeInstigatorId <-
    if instigator
      then do
        instigatorId <- getWord32Bits
        pure (Just instigatorId)
      else pure Nothing
  pickedUp <- BinaryBit.getBool
  pure (PickupAttribute instigator maybeInstigatorId pickedUp)

getQWordAttribute :: BinaryBit.BitGet AttributeValue
getQWordAttribute = do
  word64 <- getWord64Bits
  pure (QWordAttribute word64)

getReservationAttribute :: BinaryBit.BitGet AttributeValue
getReservationAttribute = do
  number <- getCompressedWord 7
  (systemId, remoteId, localId) <- getUniqueId
  name <-
    if systemId == Word8 0
      then pure Nothing
      else do
        name <- getTextBits
        pure (Just name)
  a <- BinaryBit.getBool
  b <- BinaryBit.getBool
  pure (ReservationAttribute number systemId remoteId localId name a b)

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

getTeamPaintAttribute :: BinaryBit.BitGet AttributeValue
getTeamPaintAttribute = do
  team <- getWord8Bits
  primaryColor <- getWord8Bits
  accentColor <- getWord8Bits
  primaryFinish <- getWord32Bits
  accentFinish <- getWord32Bits
  pure
    (TeamPaintAttribute team primaryColor accentColor primaryFinish accentFinish)

getUniqueIdAttribute :: BinaryBit.BitGet AttributeValue
getUniqueIdAttribute = do
  (systemId, remoteId, localId) <- getUniqueId
  pure (UniqueIdAttribute systemId remoteId localId)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
    BooleanAttribute x -> BinaryBit.putBool x
    ByteAttribute byte -> putWord8Bits byte
    CamSettingsAttribute fov height angle distance stiffness swivelSpeed -> do
      putFloat32Bits fov
      putFloat32Bits height
      putFloat32Bits angle
      putFloat32Bits distance
      putFloat32Bits stiffness
      putFloat32Bits swivelSpeed
    EnumAttribute x -> BinaryBit.putWord16be 11 x
    FlaggedIntAttribute flag int -> do
      BinaryBit.putBool flag
      putInt32Bits int
    FloatAttribute float -> putFloat32Bits float
    GameModeAttribute word8 -> BinaryBit.putWord8 8 word8
    IntAttribute int -> putInt32Bits int
    LoadoutAttribute _ _ _ _ _ _ _ _ _ -> putLoadoutAttribute value
    LoadoutOnlineAttribute _ -> putLoadoutOnlineAttribute value
    LoadoutsAttribute blueLoadout orangeLoadout -> do
      putLoadoutAttribute blueLoadout
      putLoadoutAttribute orangeLoadout
    LoadoutsOnlineAttribute blueLoadout orangeLoadout unknown1 unknown2 -> do
      putLoadoutOnlineAttribute blueLoadout
      putLoadoutOnlineAttribute orangeLoadout
      BinaryBit.putBool unknown1
      BinaryBit.putBool unknown2
    LocationAttribute location -> putLocation location
    PartyLeaderAttribute systemId maybeRemoteAndLocalId -> do
      putWord8Bits systemId
      case maybeRemoteAndLocalId of
        Nothing -> pure ()
        Just (remoteId, localId) -> do
          putRemoteId remoteId
          putWord8Bits localId
    PickupAttribute instigator maybeInstigatorId pickedUp -> do
      BinaryBit.putBool instigator
      case maybeInstigatorId of
        Nothing -> pure ()
        Just instigatorId -> putWord32Bits instigatorId
      BinaryBit.putBool pickedUp
    QWordAttribute word64 -> putWord64Bits word64
    ReservationAttribute number systemId remoteId localId maybeName a b -> do
      putCompressedWord number
      putUniqueId systemId remoteId localId
      case maybeName of
        Nothing -> pure ()
        Just name -> putTextBits name
      BinaryBit.putBool a
      BinaryBit.putBool b
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
    TeamPaintAttribute team primaryColor accentColor primaryFinish accentFinish -> do
      putWord8Bits team
      putWord8Bits primaryColor
      putWord8Bits accentColor
      putWord32Bits primaryFinish
      putWord32Bits accentFinish
    UniqueIdAttribute systemId remoteId localId ->
      putUniqueId systemId remoteId localId
    _ -> fail ("don't know how to put attribute value " ++ show value)

getUniqueId :: BinaryBit.BitGet (Word8, RemoteId, Word8)
getUniqueId = do
  systemId <- getWord8Bits
  remoteId <- getRemoteId systemId
  localId <- getWord8Bits
  pure (systemId, remoteId, localId)

putUniqueId :: Word8 -> RemoteId -> Word8 -> BinaryBit.BitPut ()
putUniqueId systemId remoteId localId = do
  putWord8Bits systemId
  putRemoteId remoteId
  putWord8Bits localId

putLoadoutAttribute :: AttributeValue -> BinaryBit.BitPut ()
putLoadoutAttribute value =
  case value of
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
    _ -> fail "putLoadoutAttribute"

putLoadoutOnlineAttribute :: AttributeValue -> BinaryBit.BitPut ()
putLoadoutOnlineAttribute value =
  case value of
    LoadoutOnlineAttribute values -> do
      putWord8Bits (Word8 (fromIntegral (length values)))
      mapM_
        (\xs -> do
           putWord8Bits (Word8 (fromIntegral (length xs)))
           mapM_
             (\(x, y) -> do
                putWord32Bits x
                putCompressedWord y)
             xs)
        values
    _ -> fail "putLoadoutOnlineAttribute"
