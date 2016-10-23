module Rattletrap.AttributeValue
  ( module Rattletrap.AttributeValue
  , module Export
  ) where

import Rattletrap.AttributeValue.Boolean as Export
import Rattletrap.AttributeValue.Byte as Export
import Rattletrap.AttributeValue.CamSettings as Export
import Rattletrap.AttributeValue.Demolish as Export
import Rattletrap.AttributeValue.Enum as Export
import Rattletrap.AttributeValue.Explosion as Export
import Rattletrap.AttributeValue.Float as Export
import Rattletrap.AttributeValue.Int as Export
import Rattletrap.AttributeValue.Location as Export
import Rattletrap.AttributeValue.QWord as Export
import Rattletrap.AttributeValue.String as Export

import Rattletrap.CompressedWord
import Rattletrap.CompressedWordVector
import Rattletrap.Float32
import Rattletrap.Int32
import Rattletrap.Int8Vector
import Rattletrap.RemoteId
import Rattletrap.Text
import Rattletrap.Vector
import Rattletrap.Word32
import Rattletrap.Word8

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Map as Map
import qualified Data.Word as Word

data AttributeValue
  = BooleanAttribute BooleanAttributeValue
  | ByteAttribute ByteAttributeValue
  | CamSettingsAttribute CamSettingsAttributeValue
  | DemolishAttribute DemolishAttributeValue
  | EnumAttribute EnumAttributeValue
  | ExplosionAttribute ExplosionAttributeValue
  | FlaggedIntAttribute Bool
                        Int32
  | FloatAttribute FloatAttributeValue
  | GameModeAttribute Int
                      Word.Word8
  | IntAttribute IntAttributeValue
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
  | LocationAttribute LocationAttributeValue
  | MusicStingerAttribute Bool
                          Word32
                          Word8
  | PartyLeaderAttribute Word8
                         (Maybe (RemoteId, Word8))
  | PickupAttribute Bool
                    (Maybe Word32)
                    Bool
  | PrivateMatchSettingsAttribute Text
                                  Word32
                                  Word32
                                  Text
                                  Text
                                  Bool
  | QWordAttribute QWordAttributeValue
  | ReservationAttribute CompressedWord
                         Word8
                         RemoteId
                         Word8
                         (Maybe Text)
                         Bool
                         Bool
                         (Maybe Word.Word8)
  | RigidBodyStateAttribute Bool
                            Vector
                            CompressedWordVector
                            (Maybe Vector)
                            (Maybe Vector)
  | StringAttribute StringAttributeValue
  | TeamPaintAttribute Word8
                       Word8
                       Word8
                       Word32
                       Word32
  | UniqueIdAttribute Word8
                      RemoteId
                      Word8
  | WeldedInfoAttribute Bool
                        Int32
                        Vector
                        Float32
                        Int8Vector
  deriving (Eq, Ord, Show)

getAttributeValue :: (Int, Int) -> Text -> BinaryBit.BitGet AttributeValue
getAttributeValue version name =
  case Map.lookup name getters of
    Nothing -> fail ("don't know how to get attribute value " ++ show name)
    Just getter -> getter version

getters :: Map.Map Text ((Int, Int) -> BinaryBit.BitGet AttributeValue)
getters =
  Map.mapKeys
    stringToText
    (Map.fromList
       [ ("Engine.Actor:bBlockActors", const getBooleanAttribute)
       , ("Engine.Actor:bCollideActors", const getBooleanAttribute)
       , ("Engine.Actor:bHidden", const getBooleanAttribute)
       , ("Engine.Actor:DrawScale", const getFloatAttribute)
       , ("Engine.Actor:Role", const getEnumAttribute)
       , ("Engine.GameReplicationInfo:bMatchIsOver", const getBooleanAttribute)
       , ("Engine.GameReplicationInfo:GameClass", const getFlaggedIntAttribute)
       , ("Engine.GameReplicationInfo:ServerName", const getStringAttribute)
       , ("Engine.Pawn:PlayerReplicationInfo", const getFlaggedIntAttribute)
       , ("Engine.PlayerReplicationInfo:bBot", const getBooleanAttribute)
       , ( "Engine.PlayerReplicationInfo:bIsSpectator"
         , const getBooleanAttribute)
       , ( "Engine.PlayerReplicationInfo:bReadyToPlay"
         , const getBooleanAttribute)
       , ( "Engine.PlayerReplicationInfo:bWaitingPlayer"
         , const getBooleanAttribute)
       , ("Engine.PlayerReplicationInfo:Ping", const getByteAttribute)
       , ("Engine.PlayerReplicationInfo:PlayerID", const getIntAttribute)
       , ("Engine.PlayerReplicationInfo:PlayerName", const getStringAttribute)
       , ( "Engine.PlayerReplicationInfo:RemoteUserData"
         , const getStringAttribute)
       , ("Engine.PlayerReplicationInfo:Score", const getIntAttribute)
       , ("Engine.PlayerReplicationInfo:Team", const getFlaggedIntAttribute)
       , ("Engine.PlayerReplicationInfo:UniqueId", const getUniqueIdAttribute)
       , ("Engine.TeamInfo:Score", const getIntAttribute)
       , ("ProjectX.GRI_X:bGameStarted", const getBooleanAttribute)
       , ("ProjectX.GRI_X:GameServerID", const getQWordAttribute)
       , ("ProjectX.GRI_X:ReplicatedGameMutatorIndex", const getIntAttribute)
       , ("ProjectX.GRI_X:ReplicatedGamePlaylist", const getIntAttribute)
       , ("ProjectX.GRI_X:Reservations", getReservationAttribute)
       , ("TAGame.Ball_TA:GameEvent", const getFlaggedIntAttribute)
       , ("TAGame.Ball_TA:HitTeamNum", const getByteAttribute)
       , ( "TAGame.Ball_TA:ReplicatedAddedCarBounceScale"
         , const getFloatAttribute)
       , ( "TAGame.Ball_TA:ReplicatedBallMaxLinearSpeedScale"
         , const getFloatAttribute)
       , ("TAGame.Ball_TA:ReplicatedBallScale", const getFloatAttribute)
       , ("TAGame.Ball_TA:ReplicatedExplosionData", const getExplosionAttribute)
       , ("TAGame.Ball_TA:ReplicatedWorldBounceScale", const getFloatAttribute)
       , ( "TAGame.CameraSettingsActor_TA:bUsingBehindView"
         , const getBooleanAttribute)
       , ( "TAGame.CameraSettingsActor_TA:bUsingSecondaryCamera"
         , const getBooleanAttribute)
       , ("TAGame.CameraSettingsActor_TA:CameraPitch", const getByteAttribute)
       , ("TAGame.CameraSettingsActor_TA:CameraYaw", const getByteAttribute)
       , ("TAGame.CameraSettingsActor_TA:PRI", const getFlaggedIntAttribute)
       , ( "TAGame.CameraSettingsActor_TA:ProfileSettings"
         , const getCamSettingsAttribute)
       , ("TAGame.Car_TA:AddedBallForceMultiplier", const getFloatAttribute)
       , ("TAGame.Car_TA:AddedCarForceMultiplier", const getFloatAttribute)
       , ("TAGame.Car_TA:AttachedPickup", const getFlaggedIntAttribute)
       , ("TAGame.Car_TA:ReplicatedDemolish", const getDemolishAttribute)
       , ("TAGame.Car_TA:TeamPaint", const getTeamPaintAttribute)
       , ( "TAGame.CarComponent_Boost_TA:bUnlimitedBoost"
         , const getBooleanAttribute)
       , ("TAGame.CarComponent_Boost_TA:bNoBoost", const getBooleanAttribute)
       , ("TAGame.CarComponent_Boost_TA:BoostModifier", const getFloatAttribute)
       , ("TAGame.CarComponent_Boost_TA:RechargeDelay", const getFloatAttribute)
       , ("TAGame.CarComponent_Boost_TA:RechargeRate", const getFloatAttribute)
       , ( "TAGame.CarComponent_Boost_TA:ReplicatedBoostAmount"
         , const getByteAttribute)
       , ( "TAGame.CarComponent_Boost_TA:UnlimitedBoostRefCount"
         , const getIntAttribute)
       , ( "TAGame.CarComponent_Dodge_TA:DodgeTorque"
         , const getLocationAttribute)
       , ( "TAGame.CarComponent_FlipCar_TA:bFlipRight"
         , const getBooleanAttribute)
       , ("TAGame.CarComponent_FlipCar_TA:FlipCarTime", const getFloatAttribute)
       , ("TAGame.CarComponent_TA:ReplicatedActive", const getByteAttribute)
       , ( "TAGame.CarComponent_TA:ReplicatedActivityTime"
         , const getFloatAttribute)
       , ("TAGame.CarComponent_TA:Vehicle", const getFlaggedIntAttribute)
       , ("TAGame.CrowdActor_TA:GameEvent", const getFlaggedIntAttribute)
       , ("TAGame.CrowdActor_TA:ModifiedNoise", const getFloatAttribute)
       , ( "TAGame.CrowdActor_TA:ReplicatedCountDownNumber"
         , const getIntAttribute)
       , ( "TAGame.CrowdActor_TA:ReplicatedOneShotSound"
         , const getFlaggedIntAttribute)
       , ( "TAGame.CrowdActor_TA:ReplicatedRoundCountDownNumber"
         , const getIntAttribute)
       , ("TAGame.CrowdManager_TA:GameEvent", const getFlaggedIntAttribute)
       , ( "TAGame.CrowdManager_TA:ReplicatedGlobalOneShotSound"
         , const getFlaggedIntAttribute)
       , ( "TAGame.GameEvent_Soccar_TA:bBallHasBeenHit"
         , const getBooleanAttribute)
       , ("TAGame.GameEvent_Soccar_TA:bOverTime", const getBooleanAttribute)
       , ( "TAGame.GameEvent_Soccar_TA:ReplicatedMusicStinger"
         , const getMusicStingerAttribute)
       , ( "TAGame.GameEvent_Soccar_TA:ReplicatedScoredOnTeam"
         , const getByteAttribute)
       , ("TAGame.GameEvent_Soccar_TA:RoundNum", const getIntAttribute)
       , ("TAGame.GameEvent_Soccar_TA:SecondsRemaining", const getIntAttribute)
       , ( "TAGame.GameEvent_Soccar_TA:SubRulesArchetype"
         , const getFlaggedIntAttribute)
       , ( "TAGame.GameEvent_SoccarPrivate_TA:MatchSettings"
         , const getPrivateMatchSettingsAttribute)
       , ("TAGame.GameEvent_TA:bCanVoteToForfeit", const getBooleanAttribute)
       , ( "TAGame.GameEvent_TA:bHasLeaveMatchPenalty"
         , const getBooleanAttribute)
       , ("TAGame.GameEvent_TA:BotSkill", const getIntAttribute)
       , ("TAGame.GameEvent_TA:GameMode", getGameModeAttribute)
       , ("TAGame.GameEvent_TA:MatchTypeClass", const getFlaggedIntAttribute)
       , ( "TAGame.GameEvent_TA:ReplicatedGameStateTimeRemaining"
         , const getIntAttribute)
       , ("TAGame.GameEvent_TA:ReplicatedStateIndex", const getByteAttribute)
       , ("TAGame.GameEvent_TA:ReplicatedStateName", const getIntAttribute)
       , ("TAGame.GameEvent_Team_TA:MaxTeamSize", const getIntAttribute)
       , ("TAGame.GRI_TA:NewDedicatedServerIP", const getStringAttribute)
       , ("TAGame.PRI_TA:bIsInSplitScreen", const getBooleanAttribute)
       , ("TAGame.PRI_TA:bMatchMVP", const getBooleanAttribute)
       , ("TAGame.PRI_TA:bOnlineLoadoutSet", const getBooleanAttribute)
       , ("TAGame.PRI_TA:bOnlineLoadoutsSet", const getBooleanAttribute)
       , ("TAGame.PRI_TA:bReady", const getBooleanAttribute)
       , ("TAGame.PRI_TA:bUsingBehindView", const getBooleanAttribute)
       , ("TAGame.PRI_TA:bUsingSecondaryCamera", const getBooleanAttribute)
       , ("TAGame.PRI_TA:CameraPitch", const getByteAttribute)
       , ("TAGame.PRI_TA:CameraSettings", const getCamSettingsAttribute)
       , ("TAGame.PRI_TA:CameraYaw", const getByteAttribute)
       , ("TAGame.PRI_TA:ClientLoadout", const getLoadoutAttribute)
       , ("TAGame.PRI_TA:ClientLoadoutOnline", const getLoadoutOnlineAttribute)
       , ("TAGame.PRI_TA:ClientLoadouts", const getLoadoutsAttribute)
       , ( "TAGame.PRI_TA:ClientLoadoutsOnline"
         , const getLoadoutsOnlineAttribute)
       , ("TAGame.PRI_TA:MatchAssists", const getIntAttribute)
       , ("TAGame.PRI_TA:MatchGoals", const getIntAttribute)
       , ("TAGame.PRI_TA:MatchSaves", const getIntAttribute)
       , ("TAGame.PRI_TA:MatchScore", const getIntAttribute)
       , ("TAGame.PRI_TA:MatchShots", const getIntAttribute)
       , ("TAGame.PRI_TA:PartyLeader", const getPartyLeaderAttribute)
       , ("TAGame.PRI_TA:PawnType", const getByteAttribute)
       , ("TAGame.PRI_TA:PersistentCamera", const getFlaggedIntAttribute)
       , ("TAGame.PRI_TA:ReplicatedGameEvent", const getFlaggedIntAttribute)
       , ("TAGame.PRI_TA:Title", const getIntAttribute)
       , ("TAGame.PRI_TA:TotalXP", const getIntAttribute)
       , ("TAGame.RBActor_TA:bFrozen", const getBooleanAttribute)
       , ("TAGame.RBActor_TA:bIgnoreSyncing", const getBooleanAttribute)
       , ("TAGame.RBActor_TA:bReplayActor", const getBooleanAttribute)
       , ( "TAGame.RBActor_TA:ReplicatedRBState"
         , const getRigidBodyStateAttribute)
       , ("TAGame.RBActor_TA:WeldedInfo", const getWeldedInfoAttribute)
       , ( "TAGame.SpecialPickup_BallFreeze_TA:RepOrigSpeed"
         , const getFloatAttribute)
       , ( "TAGame.SpecialPickup_BallVelcro_TA:AttachTime"
         , const getFloatAttribute)
       , ( "TAGame.SpecialPickup_BallVelcro_TA:bBroken"
         , const getBooleanAttribute)
       , ("TAGame.SpecialPickup_BallVelcro_TA:bHit", const getBooleanAttribute)
       , ( "TAGame.SpecialPickup_BallVelcro_TA:BreakTime"
         , const getFloatAttribute)
       , ( "TAGame.SpecialPickup_Targeted_TA:Targeted"
         , const getFlaggedIntAttribute)
       , ("TAGame.Team_Soccar_TA:GameScore", const getIntAttribute)
       , ("TAGame.Team_TA:CustomTeamName", const getStringAttribute)
       , ("TAGame.Team_TA:GameEvent", const getFlaggedIntAttribute)
       , ("TAGame.Team_TA:LogoData", const getFlaggedIntAttribute)
       , ("TAGame.Vehicle_TA:bDriving", const getBooleanAttribute)
       , ("TAGame.Vehicle_TA:bReplicatedHandbrake", const getBooleanAttribute)
       , ("TAGame.Vehicle_TA:ReplicatedSteer", const getByteAttribute)
       , ("TAGame.Vehicle_TA:ReplicatedThrottle", const getByteAttribute)
       , ( "TAGame.VehiclePickup_TA:ReplicatedPickupData"
         , const getPickupAttribute)
       ])

getBooleanAttribute :: BinaryBit.BitGet AttributeValue
getBooleanAttribute = do
  x <- getBooleanAttributeValue
  pure (BooleanAttribute x)

getByteAttribute :: BinaryBit.BitGet AttributeValue
getByteAttribute = do
  x <- getByteAttributeValue
  pure (ByteAttribute x)

getCamSettingsAttribute :: BinaryBit.BitGet AttributeValue
getCamSettingsAttribute = do
  x <- getCamSettingsAttributeValue
  pure (CamSettingsAttribute x)

getDemolishAttribute :: BinaryBit.BitGet AttributeValue
getDemolishAttribute = do
  x <- getDemolishAttributeValue
  pure (DemolishAttribute x)

getEnumAttribute :: BinaryBit.BitGet AttributeValue
getEnumAttribute = do
  x <- getEnumAttributeValue
  pure (EnumAttribute x)

getExplosionAttribute :: BinaryBit.BitGet AttributeValue
getExplosionAttribute = do
  x <- getExplosionAttributeValue
  pure (ExplosionAttribute x)

getFlaggedIntAttribute :: BinaryBit.BitGet AttributeValue
getFlaggedIntAttribute = do
  flag <- BinaryBit.getBool
  int <- getInt32Bits
  pure (FlaggedIntAttribute flag int)

getFloatAttribute :: BinaryBit.BitGet AttributeValue
getFloatAttribute = do
  x <- getFloatAttributeValue
  pure (FloatAttribute x)

getGameModeAttribute :: (Int, Int) -> BinaryBit.BitGet AttributeValue
getGameModeAttribute version = do
  let numBits =
        if beforeNeoTokyo version
          then 2
          else 8
  word8 <- BinaryBit.getWord8 numBits
  pure (GameModeAttribute numBits word8)

getIntAttribute :: BinaryBit.BitGet AttributeValue
getIntAttribute = do
  x <- getIntAttributeValue
  pure (IntAttribute x)

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
  x <- getLocationAttributeValue
  pure (LocationAttribute x)

getMusicStingerAttribute :: BinaryBit.BitGet AttributeValue
getMusicStingerAttribute = do
  flag <- BinaryBit.getBool
  cue <- getWord32Bits
  trigger <- getWord8Bits
  pure (MusicStingerAttribute flag cue trigger)

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

getPrivateMatchSettingsAttribute :: BinaryBit.BitGet AttributeValue
getPrivateMatchSettingsAttribute = do
  mutators <- getTextBits
  joinableBy <- getWord32Bits
  maxPlayers <- getWord32Bits
  gameName <- getTextBits
  password <- getTextBits
  flag <- BinaryBit.getBool
  pure
    (PrivateMatchSettingsAttribute
       mutators
       joinableBy
       maxPlayers
       gameName
       password
       flag)

getQWordAttribute :: BinaryBit.BitGet AttributeValue
getQWordAttribute = do
  x <- getQWordAttributeValue
  pure (QWordAttribute x)

getReservationAttribute :: (Int, Int) -> BinaryBit.BitGet AttributeValue
getReservationAttribute version = do
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
  mc <-
    if beforeNeoTokyo version
      then pure Nothing
      else do
        c <- BinaryBit.getWord8 6
        pure (Just c)
  pure (ReservationAttribute number systemId remoteId localId name a b mc)

getRigidBodyStateAttribute :: BinaryBit.BitGet AttributeValue
getRigidBodyStateAttribute = do
  isSleeping <- BinaryBit.getBool
  location <- getVector
  spin <- getCompressedWordVector
  linearVelocity <-
    if isSleeping
      then pure Nothing
      else do
        linearVelocity <- getVector
        pure (Just linearVelocity)
  angularVelocity <-
    if isSleeping
      then pure Nothing
      else do
        angularVelocity <- getVector
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
  x <- getStringAttributeValue
  pure (StringAttribute x)

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

getWeldedInfoAttribute :: BinaryBit.BitGet AttributeValue
getWeldedInfoAttribute = do
  active <- BinaryBit.getBool
  actorId <- getInt32Bits
  offset <- getVector
  mass <- getFloat32Bits
  rotation <- getInt8Vector
  pure (WeldedInfoAttribute active actorId offset mass rotation)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
    BooleanAttribute x -> putBooleanAttributeValue x
    ByteAttribute x -> putByteAttributeValue x
    CamSettingsAttribute x -> putCamSettingsAttributeValue x
    DemolishAttribute x -> putDemolishAttributeValue x
    EnumAttribute x -> putEnumAttributeValue x
    ExplosionAttribute x -> putExplosionAttributeValue x
    FlaggedIntAttribute flag int -> do
      BinaryBit.putBool flag
      putInt32Bits int
    FloatAttribute x -> putFloatAttributeValue x
    GameModeAttribute numBits word8 -> BinaryBit.putWord8 numBits word8
    IntAttribute x -> putIntAttributeValue x
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
    LocationAttribute x -> putLocationAttributeValue x
    MusicStingerAttribute flag cue trigger -> do
      BinaryBit.putBool flag
      putWord32Bits cue
      putWord8Bits trigger
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
    PrivateMatchSettingsAttribute mutators joinableBy maxPlayers gameName password flag -> do
      putTextBits mutators
      putWord32Bits joinableBy
      putWord32Bits maxPlayers
      putTextBits gameName
      putTextBits password
      BinaryBit.putBool flag
    QWordAttribute x -> putQWordAttributeValue x
    ReservationAttribute number systemId remoteId localId maybeName a b mc -> do
      putCompressedWord number
      putUniqueId systemId remoteId localId
      case maybeName of
        Nothing -> pure ()
        Just name -> putTextBits name
      BinaryBit.putBool a
      BinaryBit.putBool b
      case mc of
        Nothing -> pure ()
        Just c -> BinaryBit.putWord8 6 c
    RigidBodyStateAttribute isSleeping location spin maybeLinearVelocity maybeAngularVelocity -> do
      BinaryBit.putBool isSleeping
      putVector location
      putCompressedWordVector spin
      case maybeLinearVelocity of
        Nothing -> pure ()
        Just linearVelocity -> putVector linearVelocity
      case maybeAngularVelocity of
        Nothing -> pure ()
        Just angularVelocity -> putVector angularVelocity
    StringAttribute x -> putStringAttributeValue x
    TeamPaintAttribute team primaryColor accentColor primaryFinish accentFinish -> do
      putWord8Bits team
      putWord8Bits primaryColor
      putWord8Bits accentColor
      putWord32Bits primaryFinish
      putWord32Bits accentFinish
    UniqueIdAttribute systemId remoteId localId ->
      putUniqueId systemId remoteId localId
    WeldedInfoAttribute active actorId offset mass rotation -> do
      BinaryBit.putBool active
      putInt32Bits actorId
      putVector offset
      putFloat32Bits mass
      putInt8Vector rotation

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

beforeNeoTokyo :: (Int, Int) -> Bool
beforeNeoTokyo version = version < (868, 12)
