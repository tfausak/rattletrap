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
  case Map.lookup name attributeNamesToConstructors of
    Nothing -> fail ("don't know how to get attribute value " ++ show name)
    Just constructor ->
      case constructor of
        BooleanAttribute _ -> do
          x <- getBooleanAttributeValue
          pure (BooleanAttribute x)
        ByteAttribute _ -> do
          x <- getByteAttributeValue
          pure (ByteAttribute x)
        CamSettingsAttribute _ -> do
          x <- getCamSettingsAttributeValue
          pure (CamSettingsAttribute x)
        DemolishAttribute _ -> do
          x <- getDemolishAttributeValue
          pure (DemolishAttribute x)
        EnumAttribute _ -> do
          x <- getEnumAttributeValue
          pure (EnumAttribute x)
        ExplosionAttribute _ -> do
          x <- getExplosionAttributeValue
          pure (ExplosionAttribute x)
        FlaggedIntAttribute _ -> do
          x <- getFlaggedIntAttributeValue
          pure (FlaggedIntAttribute x)
        FloatAttribute _ -> do
          x <- getFloatAttributeValue
          pure (FloatAttribute x)
        GameModeAttribute _ -> do
          x <- getGameModeAttributeValue version
          pure (GameModeAttribute x)
        IntAttribute _ -> do
          x <- getIntAttributeValue
          pure (IntAttribute x)
        LoadoutAttribute _ -> do
          x <- getLoadoutAttributeValue
          pure (LoadoutAttribute x)
        LoadoutOnlineAttribute _ -> do
          x <- getLoadoutOnlineAttributeValue
          pure (LoadoutOnlineAttribute x)
        LoadoutsAttribute _ -> do
          x <- getLoadoutsAttributeValue
          pure (LoadoutsAttribute x)
        LoadoutsOnlineAttribute _ -> do
          x <- getLoadoutsOnlineAttributeValue
          pure (LoadoutsOnlineAttribute x)
        LocationAttribute _ -> do
          x <- getLocationAttributeValue
          pure (LocationAttribute x)
        MusicStingerAttribute _ -> do
          x <- getMusicStingerAttributeValue
          pure (MusicStingerAttribute x)
        PartyLeaderAttribute _ -> do
          x <- getPartyLeaderAttributeValue
          pure (PartyLeaderAttribute x)
        PickupAttribute _ -> do
          x <- getPickupAttributeValue
          pure (PickupAttribute x)
        PrivateMatchSettingsAttribute _ -> do
          x <- getPrivateMatchSettingsAttributeValue
          pure (PrivateMatchSettingsAttribute x)
        QWordAttribute _ -> do
          x <- getQWordAttributeValue
          pure (QWordAttribute x)
        ReservationAttribute _ -> do
          x <- getReservationAttributeValue version
          pure (ReservationAttribute x)
        RigidBodyStateAttribute _ -> do
          x <- getRigidBodyStateAttributeValue
          pure (RigidBodyStateAttribute x)
        StringAttribute _ -> do
          x <- getStringAttributeValue
          pure (StringAttribute x)
        TeamPaintAttribute _ -> do
          x <- getTeamPaintAttributeValue
          pure (TeamPaintAttribute x)
        UniqueIdAttribute _ -> do
          x <- getUniqueIdAttributeValue
          pure (UniqueIdAttribute x)
        WeldedInfoAttribute _ -> do
          x <- getWeldedInfoAttributeValue
          pure (WeldedInfoAttribute x)

attributeNamesToConstructors :: Map.Map Text AttributeValue
attributeNamesToConstructors =
  Map.mapKeys stringToText (Map.fromList rawAttributeNamesToConstructors)

rawAttributeNamesToConstructors :: [(String, AttributeValue)]
rawAttributeNamesToConstructors =
  [ ("Engine.Actor:bBlockActors", BooleanAttribute undefined)
  , ("Engine.Actor:bCollideActors", BooleanAttribute undefined)
  , ("Engine.Actor:bHidden", BooleanAttribute undefined)
  , ("Engine.Actor:DrawScale", FloatAttribute undefined)
  , ("Engine.Actor:Role", EnumAttribute undefined)
  , ("Engine.GameReplicationInfo:bMatchIsOver", BooleanAttribute undefined)
  , ("Engine.GameReplicationInfo:GameClass", FlaggedIntAttribute undefined)
  , ("Engine.GameReplicationInfo:ServerName", StringAttribute undefined)
  , ("Engine.Pawn:PlayerReplicationInfo", FlaggedIntAttribute undefined)
  , ("Engine.PlayerReplicationInfo:bBot", BooleanAttribute undefined)
  , ("Engine.PlayerReplicationInfo:bIsSpectator", BooleanAttribute undefined)
  , ("Engine.PlayerReplicationInfo:bReadyToPlay", BooleanAttribute undefined)
  , ("Engine.PlayerReplicationInfo:bWaitingPlayer", BooleanAttribute undefined)
  , ("Engine.PlayerReplicationInfo:Ping", ByteAttribute undefined)
  , ("Engine.PlayerReplicationInfo:PlayerID", IntAttribute undefined)
  , ("Engine.PlayerReplicationInfo:PlayerName", StringAttribute undefined)
  , ("Engine.PlayerReplicationInfo:RemoteUserData", StringAttribute undefined)
  , ("Engine.PlayerReplicationInfo:Score", IntAttribute undefined)
  , ("Engine.PlayerReplicationInfo:Team", FlaggedIntAttribute undefined)
  , ("Engine.PlayerReplicationInfo:UniqueId", UniqueIdAttribute undefined)
  , ("Engine.TeamInfo:Score", IntAttribute undefined)
  , ("ProjectX.GRI_X:bGameStarted", BooleanAttribute undefined)
  , ("ProjectX.GRI_X:GameServerID", QWordAttribute undefined)
  , ("ProjectX.GRI_X:ReplicatedGameMutatorIndex", IntAttribute undefined)
  , ("ProjectX.GRI_X:ReplicatedGamePlaylist", IntAttribute undefined)
  , ("ProjectX.GRI_X:Reservations", ReservationAttribute undefined)
  , ("TAGame.Ball_TA:GameEvent", FlaggedIntAttribute undefined)
  , ("TAGame.Ball_TA:HitTeamNum", ByteAttribute undefined)
  , ("TAGame.Ball_TA:ReplicatedAddedCarBounceScale", FloatAttribute undefined)
  , ( "TAGame.Ball_TA:ReplicatedBallMaxLinearSpeedScale"
    , FloatAttribute undefined)
  , ("TAGame.Ball_TA:ReplicatedBallScale", FloatAttribute undefined)
  , ("TAGame.Ball_TA:ReplicatedExplosionData", ExplosionAttribute undefined)
  , ("TAGame.Ball_TA:ReplicatedWorldBounceScale", FloatAttribute undefined)
  , ( "TAGame.CameraSettingsActor_TA:bUsingBehindView"
    , BooleanAttribute undefined)
  , ( "TAGame.CameraSettingsActor_TA:bUsingSecondaryCamera"
    , BooleanAttribute undefined)
  , ("TAGame.CameraSettingsActor_TA:CameraPitch", ByteAttribute undefined)
  , ("TAGame.CameraSettingsActor_TA:CameraYaw", ByteAttribute undefined)
  , ("TAGame.CameraSettingsActor_TA:PRI", FlaggedIntAttribute undefined)
  , ( "TAGame.CameraSettingsActor_TA:ProfileSettings"
    , CamSettingsAttribute undefined)
  , ("TAGame.Car_TA:AddedBallForceMultiplier", FloatAttribute undefined)
  , ("TAGame.Car_TA:AddedCarForceMultiplier", FloatAttribute undefined)
  , ("TAGame.Car_TA:AttachedPickup", FlaggedIntAttribute undefined)
  , ("TAGame.Car_TA:ReplicatedDemolish", DemolishAttribute undefined)
  , ("TAGame.Car_TA:TeamPaint", TeamPaintAttribute undefined)
  , ("TAGame.CarComponent_Boost_TA:bNoBoost", BooleanAttribute undefined)
  , ("TAGame.CarComponent_Boost_TA:BoostModifier", FloatAttribute undefined)
  , ("TAGame.CarComponent_Boost_TA:bUnlimitedBoost", BooleanAttribute undefined)
  , ("TAGame.CarComponent_Boost_TA:RechargeDelay", FloatAttribute undefined)
  , ("TAGame.CarComponent_Boost_TA:RechargeRate", FloatAttribute undefined)
  , ( "TAGame.CarComponent_Boost_TA:ReplicatedBoostAmount"
    , ByteAttribute undefined)
  , ( "TAGame.CarComponent_Boost_TA:UnlimitedBoostRefCount"
    , IntAttribute undefined)
  , ("TAGame.CarComponent_Dodge_TA:DodgeTorque", LocationAttribute undefined)
  , ("TAGame.CarComponent_FlipCar_TA:bFlipRight", BooleanAttribute undefined)
  , ("TAGame.CarComponent_FlipCar_TA:FlipCarTime", FloatAttribute undefined)
  , ("TAGame.CarComponent_TA:ReplicatedActive", ByteAttribute undefined)
  , ("TAGame.CarComponent_TA:ReplicatedActivityTime", FloatAttribute undefined)
  , ("TAGame.CarComponent_TA:Vehicle", FlaggedIntAttribute undefined)
  , ("TAGame.CrowdActor_TA:GameEvent", FlaggedIntAttribute undefined)
  , ("TAGame.CrowdActor_TA:ModifiedNoise", FloatAttribute undefined)
  , ("TAGame.CrowdActor_TA:ReplicatedCountDownNumber", IntAttribute undefined)
  , ( "TAGame.CrowdActor_TA:ReplicatedOneShotSound"
    , FlaggedIntAttribute undefined)
  , ( "TAGame.CrowdActor_TA:ReplicatedRoundCountDownNumber"
    , IntAttribute undefined)
  , ("TAGame.CrowdManager_TA:GameEvent", FlaggedIntAttribute undefined)
  , ( "TAGame.CrowdManager_TA:ReplicatedGlobalOneShotSound"
    , FlaggedIntAttribute undefined)
  , ("TAGame.GameEvent_Soccar_TA:bBallHasBeenHit", BooleanAttribute undefined)
  , ("TAGame.GameEvent_Soccar_TA:bOverTime", BooleanAttribute undefined)
  , ( "TAGame.GameEvent_Soccar_TA:ReplicatedMusicStinger"
    , MusicStingerAttribute undefined)
  , ( "TAGame.GameEvent_Soccar_TA:ReplicatedScoredOnTeam"
    , ByteAttribute undefined)
  , ("TAGame.GameEvent_Soccar_TA:RoundNum", IntAttribute undefined)
  , ("TAGame.GameEvent_Soccar_TA:SecondsRemaining", IntAttribute undefined)
  , ( "TAGame.GameEvent_Soccar_TA:SubRulesArchetype"
    , FlaggedIntAttribute undefined)
  , ( "TAGame.GameEvent_SoccarPrivate_TA:MatchSettings"
    , PrivateMatchSettingsAttribute undefined)
  , ("TAGame.GameEvent_TA:bCanVoteToForfeit", BooleanAttribute undefined)
  , ("TAGame.GameEvent_TA:bHasLeaveMatchPenalty", BooleanAttribute undefined)
  , ("TAGame.GameEvent_TA:BotSkill", IntAttribute undefined)
  , ("TAGame.GameEvent_TA:GameMode", GameModeAttribute undefined)
  , ("TAGame.GameEvent_TA:MatchTypeClass", FlaggedIntAttribute undefined)
  , ( "TAGame.GameEvent_TA:ReplicatedGameStateTimeRemaining"
    , IntAttribute undefined)
  , ("TAGame.GameEvent_TA:ReplicatedStateIndex", ByteAttribute undefined)
  , ("TAGame.GameEvent_TA:ReplicatedStateName", IntAttribute undefined)
  , ("TAGame.GameEvent_Team_TA:MaxTeamSize", IntAttribute undefined)
  , ("TAGame.GRI_TA:NewDedicatedServerIP", StringAttribute undefined)
  , ("TAGame.PRI_TA:bIsInSplitScreen", BooleanAttribute undefined)
  , ("TAGame.PRI_TA:bMatchMVP", BooleanAttribute undefined)
  , ("TAGame.PRI_TA:bOnlineLoadoutSet", BooleanAttribute undefined)
  , ("TAGame.PRI_TA:bOnlineLoadoutsSet", BooleanAttribute undefined)
  , ("TAGame.PRI_TA:bReady", BooleanAttribute undefined)
  , ("TAGame.PRI_TA:bUsingBehindView", BooleanAttribute undefined)
  , ("TAGame.PRI_TA:bUsingSecondaryCamera", BooleanAttribute undefined)
  , ("TAGame.PRI_TA:CameraPitch", ByteAttribute undefined)
  , ("TAGame.PRI_TA:CameraSettings", CamSettingsAttribute undefined)
  , ("TAGame.PRI_TA:CameraYaw", ByteAttribute undefined)
  , ("TAGame.PRI_TA:ClientLoadout", LoadoutAttribute undefined)
  , ("TAGame.PRI_TA:ClientLoadoutOnline", LoadoutOnlineAttribute undefined)
  , ("TAGame.PRI_TA:ClientLoadouts", LoadoutsAttribute undefined)
  , ("TAGame.PRI_TA:ClientLoadoutsOnline", LoadoutsOnlineAttribute undefined)
  , ("TAGame.PRI_TA:MatchAssists", IntAttribute undefined)
  , ("TAGame.PRI_TA:MatchGoals", IntAttribute undefined)
  , ("TAGame.PRI_TA:MatchSaves", IntAttribute undefined)
  , ("TAGame.PRI_TA:MatchScore", IntAttribute undefined)
  , ("TAGame.PRI_TA:MatchShots", IntAttribute undefined)
  , ("TAGame.PRI_TA:PartyLeader", PartyLeaderAttribute undefined)
  , ("TAGame.PRI_TA:PawnType", ByteAttribute undefined)
  , ("TAGame.PRI_TA:PersistentCamera", FlaggedIntAttribute undefined)
  , ("TAGame.PRI_TA:ReplicatedGameEvent", FlaggedIntAttribute undefined)
  , ("TAGame.PRI_TA:Title", IntAttribute undefined)
  , ("TAGame.PRI_TA:TotalXP", IntAttribute undefined)
  , ("TAGame.RBActor_TA:bFrozen", BooleanAttribute undefined)
  , ("TAGame.RBActor_TA:bIgnoreSyncing", BooleanAttribute undefined)
  , ("TAGame.RBActor_TA:bReplayActor", BooleanAttribute undefined)
  , ("TAGame.RBActor_TA:ReplicatedRBState", RigidBodyStateAttribute undefined)
  , ("TAGame.RBActor_TA:WeldedInfo", WeldedInfoAttribute undefined)
  , ( "TAGame.SpecialPickup_BallFreeze_TA:RepOrigSpeed"
    , FloatAttribute undefined)
  , ("TAGame.SpecialPickup_BallVelcro_TA:AttachTime", FloatAttribute undefined)
  , ("TAGame.SpecialPickup_BallVelcro_TA:bBroken", BooleanAttribute undefined)
  , ("TAGame.SpecialPickup_BallVelcro_TA:bHit", BooleanAttribute undefined)
  , ("TAGame.SpecialPickup_BallVelcro_TA:BreakTime", FloatAttribute undefined)
  , ("TAGame.SpecialPickup_Targeted_TA:Targeted", FlaggedIntAttribute undefined)
  , ("TAGame.Team_Soccar_TA:GameScore", IntAttribute undefined)
  , ("TAGame.Team_TA:CustomTeamName", StringAttribute undefined)
  , ("TAGame.Team_TA:GameEvent", FlaggedIntAttribute undefined)
  , ("TAGame.Team_TA:LogoData", FlaggedIntAttribute undefined)
  , ("TAGame.Vehicle_TA:bDriving", BooleanAttribute undefined)
  , ("TAGame.Vehicle_TA:bReplicatedHandbrake", BooleanAttribute undefined)
  , ("TAGame.Vehicle_TA:ReplicatedSteer", ByteAttribute undefined)
  , ("TAGame.Vehicle_TA:ReplicatedThrottle", ByteAttribute undefined)
  , ("TAGame.VehiclePickup_TA:ReplicatedPickupData", PickupAttribute undefined)
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
