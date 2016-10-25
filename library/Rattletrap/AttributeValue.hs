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
import Rattletrap.AttributeValue.FlaggedInt as Export
import Rattletrap.AttributeValue.Float as Export
import Rattletrap.AttributeValue.GameMode as Export
import Rattletrap.AttributeValue.Int as Export
import Rattletrap.AttributeValue.LoadoutOnline as Export
import Rattletrap.AttributeValue.LoadoutsOnline as Export
import Rattletrap.AttributeValue.Location as Export
import Rattletrap.AttributeValue.MusicStinger as Export
import Rattletrap.AttributeValue.PartyLeader as Export
import Rattletrap.AttributeValue.Pickup as Export
import Rattletrap.AttributeValue.PrivateMatchSettings as Export
import Rattletrap.AttributeValue.QWord as Export
import Rattletrap.AttributeValue.Reservation as Export
import Rattletrap.AttributeValue.RigidBodyState as Export
import Rattletrap.AttributeValue.String as Export
import Rattletrap.AttributeValue.TeamPaint as Export
import Rattletrap.AttributeValue.UniqueId as Export
import Rattletrap.AttributeValue.WeldedInfo as Export

import Rattletrap.Text
import Rattletrap.Word32
import Rattletrap.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Map as Map

data AttributeValue
  = BooleanAttribute BooleanAttributeValue
  | ByteAttribute ByteAttributeValue
  | CamSettingsAttribute CamSettingsAttributeValue
  | DemolishAttribute DemolishAttributeValue
  | EnumAttribute EnumAttributeValue
  | ExplosionAttribute ExplosionAttributeValue
  | FlaggedIntAttribute FlaggedIntAttributeValue
  | FloatAttribute FloatAttributeValue
  | GameModeAttribute GameModeAttributeValue
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
  | LoadoutOnlineAttribute LoadoutOnlineAttributeValue
  | LoadoutsAttribute AttributeValue
                      AttributeValue
  | LoadoutsOnlineAttribute LoadoutsOnlineAttributeValue
  | LocationAttribute LocationAttributeValue
  | MusicStingerAttribute MusicStingerAttributeValue
  | PartyLeaderAttribute PartyLeaderAttributeValue
  | PickupAttribute PickupAttributeValue
  | PrivateMatchSettingsAttribute PrivateMatchSettingsAttributeValue
  | QWordAttribute QWordAttributeValue
  | ReservationAttribute ReservationAttributeValue
  | RigidBodyStateAttribute RigidBodyStateAttributeValue
  | StringAttribute StringAttributeValue
  | TeamPaintAttribute TeamPaintAttributeValue
  | UniqueIdAttribute UniqueIdAttributeValue
  | WeldedInfoAttribute WeldedInfoAttributeValue
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
  x <- getFlaggedIntAttributeValue
  pure (FlaggedIntAttribute x)

getFloatAttribute :: BinaryBit.BitGet AttributeValue
getFloatAttribute = do
  x <- getFloatAttributeValue
  pure (FloatAttribute x)

getGameModeAttribute :: (Int, Int) -> BinaryBit.BitGet AttributeValue
getGameModeAttribute version = do
  x <- getGameModeAttributeValue version
  pure (GameModeAttribute x)

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
  x <- getLoadoutOnlineAttributeValue
  pure (LoadoutOnlineAttribute x)

getLoadoutsAttribute :: BinaryBit.BitGet AttributeValue
getLoadoutsAttribute = do
  blueLoadout <- getLoadoutAttribute
  orangeLoadout <- getLoadoutAttribute
  pure (LoadoutsAttribute blueLoadout orangeLoadout)

getLoadoutsOnlineAttribute :: BinaryBit.BitGet AttributeValue
getLoadoutsOnlineAttribute = do
  x <- getLoadoutsOnlineAttributeValue
  pure (LoadoutsOnlineAttribute x)

getLocationAttribute :: BinaryBit.BitGet AttributeValue
getLocationAttribute = do
  x <- getLocationAttributeValue
  pure (LocationAttribute x)

getMusicStingerAttribute :: BinaryBit.BitGet AttributeValue
getMusicStingerAttribute = do
  x <- getMusicStingerAttributeValue
  pure (MusicStingerAttribute x)

getPartyLeaderAttribute :: BinaryBit.BitGet AttributeValue
getPartyLeaderAttribute = do
  x <- getPartyLeaderAttributeValue
  pure (PartyLeaderAttribute x)

getPickupAttribute :: BinaryBit.BitGet AttributeValue
getPickupAttribute = do
  x <- getPickupAttributeValue
  pure (PickupAttribute x)

getPrivateMatchSettingsAttribute :: BinaryBit.BitGet AttributeValue
getPrivateMatchSettingsAttribute = do
  x <- getPrivateMatchSettingsAttributeValue
  pure (PrivateMatchSettingsAttribute x)

getQWordAttribute :: BinaryBit.BitGet AttributeValue
getQWordAttribute = do
  x <- getQWordAttributeValue
  pure (QWordAttribute x)

getReservationAttribute :: (Int, Int) -> BinaryBit.BitGet AttributeValue
getReservationAttribute version = do
  x <- getReservationAttributeValue version
  pure (ReservationAttribute x)

getRigidBodyStateAttribute :: BinaryBit.BitGet AttributeValue
getRigidBodyStateAttribute = do
  x <- getRigidBodyStateAttributeValue
  pure (RigidBodyStateAttribute x)

getStringAttribute :: BinaryBit.BitGet AttributeValue
getStringAttribute = do
  x <- getStringAttributeValue
  pure (StringAttribute x)

getTeamPaintAttribute :: BinaryBit.BitGet AttributeValue
getTeamPaintAttribute = do
  x <- getTeamPaintAttributeValue
  pure (TeamPaintAttribute x)

getUniqueIdAttribute :: BinaryBit.BitGet AttributeValue
getUniqueIdAttribute = do
  x <- getUniqueIdAttributeValue
  pure (UniqueIdAttribute x)

getWeldedInfoAttribute :: BinaryBit.BitGet AttributeValue
getWeldedInfoAttribute = do
  x <- getWeldedInfoAttributeValue
  pure (WeldedInfoAttribute x)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
    BooleanAttribute x -> putBooleanAttributeValue x
    ByteAttribute x -> putByteAttributeValue x
    CamSettingsAttribute x -> putCamSettingsAttributeValue x
    DemolishAttribute x -> putDemolishAttributeValue x
    EnumAttribute x -> putEnumAttributeValue x
    ExplosionAttribute x -> putExplosionAttributeValue x
    FlaggedIntAttribute x -> putFlaggedIntAttributeValue x
    FloatAttribute x -> putFloatAttributeValue x
    GameModeAttribute x -> putGameModeAttributeValue x
    IntAttribute x -> putIntAttributeValue x
    LoadoutAttribute _ _ _ _ _ _ _ _ _ -> putLoadoutAttribute value
    LoadoutOnlineAttribute x -> putLoadoutOnlineAttributeValue x
    LoadoutsAttribute blueLoadout orangeLoadout -> do
      putLoadoutAttribute blueLoadout
      putLoadoutAttribute orangeLoadout
    LoadoutsOnlineAttribute x -> putLoadoutsOnlineAttributeValue x
    LocationAttribute x -> putLocationAttributeValue x
    MusicStingerAttribute x -> putMusicStingerAttributeValue x
    PartyLeaderAttribute x -> putPartyLeaderAttributeValue x
    PickupAttribute x -> putPickupAttributeValue x
    PrivateMatchSettingsAttribute x -> putPrivateMatchSettingsAttributeValue x
    QWordAttribute x -> putQWordAttributeValue x
    ReservationAttribute x -> putReservationAttributeValue x
    RigidBodyStateAttribute x -> putRigidBodyStateAttributeValue x
    StringAttribute x -> putStringAttributeValue x
    TeamPaintAttribute x -> putTeamPaintAttributeValue x
    UniqueIdAttribute x -> putUniqueIdAttributeValue x
    WeldedInfoAttribute x -> putWeldedInfoAttributeValue x

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
