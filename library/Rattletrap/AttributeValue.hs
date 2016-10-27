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
import Rattletrap.AttributeValue.Loadout as Export
import Rattletrap.AttributeValue.LoadoutOnline as Export
import Rattletrap.AttributeValue.Loadouts as Export
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

import Rattletrap.AttributeValueType
import Rattletrap.Text

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
  | LoadoutAttribute LoadoutAttributeValue
  | LoadoutOnlineAttribute LoadoutOnlineAttributeValue
  | LoadoutsAttribute LoadoutsAttributeValue
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
  case Map.lookup name attributeValueTypes of
    Just constructor ->
      case constructor of
        AVBoolean -> do
          x <- getBooleanAttributeValue
          pure (BooleanAttribute x)
        AVByte -> do
          x <- getByteAttributeValue
          pure (ByteAttribute x)
        AVCamSettings -> do
          x <- getCamSettingsAttributeValue
          pure (CamSettingsAttribute x)
        AVDemolish -> do
          x <- getDemolishAttributeValue
          pure (DemolishAttribute x)
        AVEnum -> do
          x <- getEnumAttributeValue
          pure (EnumAttribute x)
        AVExplosion -> do
          x <- getExplosionAttributeValue
          pure (ExplosionAttribute x)
        AVFlaggedInt -> do
          x <- getFlaggedIntAttributeValue
          pure (FlaggedIntAttribute x)
        AVFloat -> do
          x <- getFloatAttributeValue
          pure (FloatAttribute x)
        AVGameMode -> do
          x <- getGameModeAttributeValue version
          pure (GameModeAttribute x)
        AVInt -> do
          x <- getIntAttributeValue
          pure (IntAttribute x)
        AVLoadout -> do
          x <- getLoadoutAttributeValue
          pure (LoadoutAttribute x)
        AVLoadoutOnline -> do
          x <- getLoadoutOnlineAttributeValue
          pure (LoadoutOnlineAttribute x)
        AVLoadouts -> do
          x <- getLoadoutsAttributeValue
          pure (LoadoutsAttribute x)
        AVLoadoutsOnline -> do
          x <- getLoadoutsOnlineAttributeValue
          pure (LoadoutsOnlineAttribute x)
        AVLocation -> do
          x <- getLocationAttributeValue
          pure (LocationAttribute x)
        AVMusicStinger -> do
          x <- getMusicStingerAttributeValue
          pure (MusicStingerAttribute x)
        AVPartyLeader -> do
          x <- getPartyLeaderAttributeValue
          pure (PartyLeaderAttribute x)
        AVPickup -> do
          x <- getPickupAttributeValue
          pure (PickupAttribute x)
        AVPrivateMatchSettings -> do
          x <- getPrivateMatchSettingsAttributeValue
          pure (PrivateMatchSettingsAttribute x)
        AVQWord -> do
          x <- getQWordAttributeValue
          pure (QWordAttribute x)
        AVReservation -> do
          x <- getReservationAttributeValue version
          pure (ReservationAttribute x)
        AVRigidBodyState -> do
          x <- getRigidBodyStateAttributeValue
          pure (RigidBodyStateAttribute x)
        AVString -> do
          x <- getStringAttributeValue
          pure (StringAttribute x)
        AVTeamPaint -> do
          x <- getTeamPaintAttributeValue
          pure (TeamPaintAttribute x)
        AVUniqueId -> do
          x <- getUniqueIdAttributeValue
          pure (UniqueIdAttribute x)
        AVWeldedInfo -> do
          x <- getWeldedInfoAttributeValue
          pure (WeldedInfoAttribute x)
    Nothing -> fail ("don't know how to get attribute value " ++ show name)

attributeValueTypes :: Map.Map Text AttributeValueType
attributeValueTypes =
  Map.mapKeys stringToText (Map.fromList rawAttributeNamesToConstructors)

rawAttributeNamesToConstructors :: [(String, AttributeValueType)]
rawAttributeNamesToConstructors =
  [ ("Engine.Actor:bBlockActors", AVBoolean)
  , ("Engine.Actor:bCollideActors", AVBoolean)
  , ("Engine.Actor:bHidden", AVBoolean)
  , ("Engine.Actor:DrawScale", AVFloat)
  , ("Engine.Actor:Role", AVEnum)
  , ("Engine.GameReplicationInfo:bMatchIsOver", AVBoolean)
  , ("Engine.GameReplicationInfo:GameClass", AVFlaggedInt)
  , ("Engine.GameReplicationInfo:ServerName", AVString)
  , ("Engine.Pawn:PlayerReplicationInfo", AVFlaggedInt)
  , ("Engine.PlayerReplicationInfo:bBot", AVBoolean)
  , ("Engine.PlayerReplicationInfo:bIsSpectator", AVBoolean)
  , ("Engine.PlayerReplicationInfo:bReadyToPlay", AVBoolean)
  , ("Engine.PlayerReplicationInfo:bWaitingPlayer", AVBoolean)
  , ("Engine.PlayerReplicationInfo:Ping", AVByte)
  , ("Engine.PlayerReplicationInfo:PlayerID", AVInt)
  , ("Engine.PlayerReplicationInfo:PlayerName", AVString)
  , ("Engine.PlayerReplicationInfo:RemoteUserData", AVString)
  , ("Engine.PlayerReplicationInfo:Score", AVInt)
  , ("Engine.PlayerReplicationInfo:Team", AVFlaggedInt)
  , ("Engine.PlayerReplicationInfo:UniqueId", AVUniqueId)
  , ("Engine.TeamInfo:Score", AVInt)
  , ("ProjectX.GRI_X:bGameStarted", AVBoolean)
  , ("ProjectX.GRI_X:GameServerID", AVQWord)
  , ("ProjectX.GRI_X:ReplicatedGameMutatorIndex", AVInt)
  , ("ProjectX.GRI_X:ReplicatedGamePlaylist", AVInt)
  , ("ProjectX.GRI_X:Reservations", AVReservation)
  , ("TAGame.Ball_TA:GameEvent", AVFlaggedInt)
  , ("TAGame.Ball_TA:HitTeamNum", AVByte)
  , ("TAGame.Ball_TA:ReplicatedAddedCarBounceScale", AVFloat)
  , ("TAGame.Ball_TA:ReplicatedBallMaxLinearSpeedScale", AVFloat)
  , ("TAGame.Ball_TA:ReplicatedBallScale", AVFloat)
  , ("TAGame.Ball_TA:ReplicatedExplosionData", AVExplosion)
  , ("TAGame.Ball_TA:ReplicatedWorldBounceScale", AVFloat)
  , ("TAGame.CameraSettingsActor_TA:bUsingBehindView", AVBoolean)
  , ("TAGame.CameraSettingsActor_TA:bUsingSecondaryCamera", AVBoolean)
  , ("TAGame.CameraSettingsActor_TA:CameraPitch", AVByte)
  , ("TAGame.CameraSettingsActor_TA:CameraYaw", AVByte)
  , ("TAGame.CameraSettingsActor_TA:PRI", AVFlaggedInt)
  , ("TAGame.CameraSettingsActor_TA:ProfileSettings", AVCamSettings)
  , ("TAGame.Car_TA:AddedBallForceMultiplier", AVFloat)
  , ("TAGame.Car_TA:AddedCarForceMultiplier", AVFloat)
  , ("TAGame.Car_TA:AttachedPickup", AVFlaggedInt)
  , ("TAGame.Car_TA:ReplicatedDemolish", AVDemolish)
  , ("TAGame.Car_TA:TeamPaint", AVTeamPaint)
  , ("TAGame.CarComponent_Boost_TA:bNoBoost", AVBoolean)
  , ("TAGame.CarComponent_Boost_TA:BoostModifier", AVFloat)
  , ("TAGame.CarComponent_Boost_TA:bUnlimitedBoost", AVBoolean)
  , ("TAGame.CarComponent_Boost_TA:RechargeDelay", AVFloat)
  , ("TAGame.CarComponent_Boost_TA:RechargeRate", AVFloat)
  , ("TAGame.CarComponent_Boost_TA:ReplicatedBoostAmount", AVByte)
  , ("TAGame.CarComponent_Boost_TA:UnlimitedBoostRefCount", AVInt)
  , ("TAGame.CarComponent_Dodge_TA:DodgeTorque", AVLocation)
  , ("TAGame.CarComponent_FlipCar_TA:bFlipRight", AVBoolean)
  , ("TAGame.CarComponent_FlipCar_TA:FlipCarTime", AVFloat)
  , ("TAGame.CarComponent_TA:ReplicatedActive", AVByte)
  , ("TAGame.CarComponent_TA:ReplicatedActivityTime", AVFloat)
  , ("TAGame.CarComponent_TA:Vehicle", AVFlaggedInt)
  , ("TAGame.CrowdActor_TA:GameEvent", AVFlaggedInt)
  , ("TAGame.CrowdActor_TA:ModifiedNoise", AVFloat)
  , ("TAGame.CrowdActor_TA:ReplicatedCountDownNumber", AVInt)
  , ("TAGame.CrowdActor_TA:ReplicatedOneShotSound", AVFlaggedInt)
  , ("TAGame.CrowdActor_TA:ReplicatedRoundCountDownNumber", AVInt)
  , ("TAGame.CrowdManager_TA:GameEvent", AVFlaggedInt)
  , ("TAGame.CrowdManager_TA:ReplicatedGlobalOneShotSound", AVFlaggedInt)
  , ("TAGame.GameEvent_Soccar_TA:bBallHasBeenHit", AVBoolean)
  , ("TAGame.GameEvent_Soccar_TA:bOverTime", AVBoolean)
  , ("TAGame.GameEvent_Soccar_TA:ReplicatedMusicStinger", AVMusicStinger)
  , ("TAGame.GameEvent_Soccar_TA:ReplicatedScoredOnTeam", AVByte)
  , ("TAGame.GameEvent_Soccar_TA:RoundNum", AVInt)
  , ("TAGame.GameEvent_Soccar_TA:SecondsRemaining", AVInt)
  , ("TAGame.GameEvent_Soccar_TA:SubRulesArchetype", AVFlaggedInt)
  , ("TAGame.GameEvent_SoccarPrivate_TA:MatchSettings", AVPrivateMatchSettings)
  , ("TAGame.GameEvent_TA:bCanVoteToForfeit", AVBoolean)
  , ("TAGame.GameEvent_TA:bHasLeaveMatchPenalty", AVBoolean)
  , ("TAGame.GameEvent_TA:BotSkill", AVInt)
  , ("TAGame.GameEvent_TA:GameMode", AVGameMode)
  , ("TAGame.GameEvent_TA:MatchTypeClass", AVFlaggedInt)
  , ("TAGame.GameEvent_TA:ReplicatedGameStateTimeRemaining", AVInt)
  , ("TAGame.GameEvent_TA:ReplicatedStateIndex", AVByte)
  , ("TAGame.GameEvent_TA:ReplicatedStateName", AVInt)
  , ("TAGame.GameEvent_Team_TA:MaxTeamSize", AVInt)
  , ("TAGame.GRI_TA:NewDedicatedServerIP", AVString)
  , ("TAGame.PRI_TA:bIsInSplitScreen", AVBoolean)
  , ("TAGame.PRI_TA:bMatchMVP", AVBoolean)
  , ("TAGame.PRI_TA:bOnlineLoadoutSet", AVBoolean)
  , ("TAGame.PRI_TA:bOnlineLoadoutsSet", AVBoolean)
  , ("TAGame.PRI_TA:bReady", AVBoolean)
  , ("TAGame.PRI_TA:bUsingBehindView", AVBoolean)
  , ("TAGame.PRI_TA:bUsingSecondaryCamera", AVBoolean)
  , ("TAGame.PRI_TA:CameraPitch", AVByte)
  , ("TAGame.PRI_TA:CameraSettings", AVCamSettings)
  , ("TAGame.PRI_TA:CameraYaw", AVByte)
  , ("TAGame.PRI_TA:ClientLoadout", AVLoadout)
  , ("TAGame.PRI_TA:ClientLoadoutOnline", AVLoadoutOnline)
  , ("TAGame.PRI_TA:ClientLoadouts", AVLoadouts)
  , ("TAGame.PRI_TA:ClientLoadoutsOnline", AVLoadoutsOnline)
  , ("TAGame.PRI_TA:MatchAssists", AVInt)
  , ("TAGame.PRI_TA:MatchGoals", AVInt)
  , ("TAGame.PRI_TA:MatchSaves", AVInt)
  , ("TAGame.PRI_TA:MatchScore", AVInt)
  , ("TAGame.PRI_TA:MatchShots", AVInt)
  , ("TAGame.PRI_TA:PartyLeader", AVPartyLeader)
  , ("TAGame.PRI_TA:PawnType", AVByte)
  , ("TAGame.PRI_TA:PersistentCamera", AVFlaggedInt)
  , ("TAGame.PRI_TA:ReplicatedGameEvent", AVFlaggedInt)
  , ("TAGame.PRI_TA:Title", AVInt)
  , ("TAGame.PRI_TA:TotalXP", AVInt)
  , ("TAGame.RBActor_TA:bFrozen", AVBoolean)
  , ("TAGame.RBActor_TA:bIgnoreSyncing", AVBoolean)
  , ("TAGame.RBActor_TA:bReplayActor", AVBoolean)
  , ("TAGame.RBActor_TA:ReplicatedRBState", AVRigidBodyState)
  , ("TAGame.RBActor_TA:WeldedInfo", AVWeldedInfo)
  , ("TAGame.SpecialPickup_BallFreeze_TA:RepOrigSpeed", AVFloat)
  , ("TAGame.SpecialPickup_BallVelcro_TA:AttachTime", AVFloat)
  , ("TAGame.SpecialPickup_BallVelcro_TA:bBroken", AVBoolean)
  , ("TAGame.SpecialPickup_BallVelcro_TA:bHit", AVBoolean)
  , ("TAGame.SpecialPickup_BallVelcro_TA:BreakTime", AVFloat)
  , ("TAGame.SpecialPickup_Targeted_TA:Targeted", AVFlaggedInt)
  , ("TAGame.Team_Soccar_TA:GameScore", AVInt)
  , ("TAGame.Team_TA:CustomTeamName", AVString)
  , ("TAGame.Team_TA:GameEvent", AVFlaggedInt)
  , ("TAGame.Team_TA:LogoData", AVFlaggedInt)
  , ("TAGame.Vehicle_TA:bDriving", AVBoolean)
  , ("TAGame.Vehicle_TA:bReplicatedHandbrake", AVBoolean)
  , ("TAGame.Vehicle_TA:ReplicatedSteer", AVByte)
  , ("TAGame.Vehicle_TA:ReplicatedThrottle", AVByte)
  , ("TAGame.VehiclePickup_TA:ReplicatedPickupData", AVPickup)
  ]

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
    LoadoutAttribute x -> putLoadoutAttributeValue x
    LoadoutOnlineAttribute x -> putLoadoutOnlineAttributeValue x
    LoadoutsAttribute x -> putLoadoutsAttributeValue x
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
