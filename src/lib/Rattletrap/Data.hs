-- brittany --columns 120
module Rattletrap.Data
  ( rawParentClasses
  , rawClassesWithLocation
  , rawClassesWithRotation
  , rawObjectClasses
  , rawAttributeTypes
  ) where

import Rattletrap.Type.AttributeType

rawParentClasses :: [(String, String)]
rawParentClasses =
  [ ("Engine.Actor", "Core.Object")
  , ("Engine.GameReplicationInfo", "Engine.ReplicationInfo")
  , ("Engine.Info", "Engine.Actor")
  , ("Engine.Pawn", "Engine.Actor")
  , ("Engine.PlayerReplicationInfo", "Engine.ReplicationInfo")
  , ("Engine.ReplicationInfo", "Engine.Info")
  , ("Engine.TeamInfo", "Engine.ReplicationInfo")
  , ("ProjectX.GRI_X", "Engine.GameReplicationInfo")
  , ("ProjectX.Pawn_X", "Engine.Pawn")
  , ("ProjectX.PRI_X", "Engine.PlayerReplicationInfo")
  , ("TAGame.Ball_God_TA", "TAGame.Ball_TA")
  , ("TAGame.Ball_Haunted_TA", "TAGame.Ball_TA")
  , ("TAGame.Ball_TA", "TAGame.RBActor_TA")
  , ("TAGame.CameraSettingsActor_TA", "Engine.ReplicationInfo")
  , ("TAGame.Car_Season_TA", "TAGame.PRI_TA")
  , ("TAGame.Car_TA", "TAGame.Vehicle_TA")
  , ("TAGame.CarComponent_Boost_TA", "TAGame.CarComponent_TA")
  , ("TAGame.CarComponent_Dodge_TA", "TAGame.CarComponent_TA")
  , ("TAGame.CarComponent_DoubleJump_TA", "TAGame.CarComponent_TA")
  , ("TAGame.CarComponent_FlipCar_TA", "TAGame.CarComponent_TA")
  , ("TAGame.CarComponent_Jump_TA", "TAGame.CarComponent_TA")
  , ("TAGame.CarComponent_TA", "Engine.ReplicationInfo")
  , ("TAGame.CrowdActor_TA", "Engine.ReplicationInfo")
  , ("TAGame.CrowdManager_TA", "Engine.ReplicationInfo")
  , ("TAGame.GameEvent_Football_TA", "TAGame.GameEvent_Soccar_TA")
  , ("TAGame.GameEvent_GodBall_TA", "TAGame.GameEvent_Soccar_TA")
  , ("TAGame.GameEvent_Season_TA", "TAGame.GameEvent_Soccar_TA")
  , ("TAGame.GameEvent_Soccar_TA", "TAGame.GameEvent_Team_TA")
  , ("TAGame.GameEvent_SoccarPrivate_TA", "TAGame.GameEvent_Soccar_TA")
  , ("TAGame.GameEvent_SoccarSplitscreen_TA", "TAGame.GameEvent_SoccarPrivate_TA")
  , ("TAGame.GameEvent_TA", "Engine.ReplicationInfo")
  , ("TAGame.GameEvent_Team_TA", "TAGame.GameEvent_TA")
  , ("TAGame.GRI_TA", "ProjectX.GRI_X")
  , ("TAGame.HauntedBallTrapTrigger_TA", "Engine.Actor")
  , ("TAGame.InMapScoreboard_TA", "Engine.Actor")
  , ("TAGame.PRI_TA", "ProjectX.PRI_X")
  , ("TAGame.RBActor_TA", "ProjectX.Pawn_X")
  , ("TAGame.SpecialPickup_BallCarSpring_TA", "TAGame.SpecialPickup_Spring_TA")
  , ("TAGame.SpecialPickup_BallFreeze_TA", "TAGame.SpecialPickup_Targeted_TA")
  , ("TAGame.SpecialPickup_BallGravity_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_BallLasso_TA", "TAGame.SpecialPickup_GrapplingHook_TA")
  , ("TAGame.SpecialPickup_Football_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_BallVelcro_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_Batarang_TA", "TAGame.SpecialPickup_BallLasso_TA")
  , ("TAGame.SpecialPickup_BoostOverride_TA", "TAGame.SpecialPickup_Targeted_TA")
  , ("TAGame.SpecialPickup_GrapplingHook_TA", "TAGame.SpecialPickup_Targeted_TA")
  , ("TAGame.SpecialPickup_HauntedBallBeam_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_HitForce_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_Rugby_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_Spring_TA", "TAGame.SpecialPickup_Targeted_TA")
  , ("TAGame.SpecialPickup_Swapper_TA", "TAGame.SpecialPickup_Targeted_TA")
  , ("TAGame.SpecialPickup_TA", "TAGame.CarComponent_TA")
  , ("TAGame.SpecialPickup_Targeted_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_Tornado_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.Team_Soccar_TA", "TAGame.Team_TA")
  , ("TAGame.Team_TA", "Engine.TeamInfo")
  , ("TAGame.Vehicle_TA", "TAGame.RBActor_TA")
  , ("TAGame.VehiclePickup_Boost_TA", "TAGame.VehiclePickup_TA")
  , ("TAGame.VehiclePickup_TA", "Engine.ReplicationInfo")
  ]

rawClassesWithLocation :: [String]
rawClassesWithLocation =
  [ "Archetypes.Ball.Ball_BasketBall_Mutator"
  , "Archetypes.Ball.Ball_Basketball"
  , "Archetypes.Ball.Ball_BasketBall"
  , "Archetypes.Ball.Ball_Breakout"
  , "Archetypes.Ball.Ball_Default"
  , "Archetypes.Ball.Ball_Puck"
  , "Archetypes.Ball.CubeBall"
  , "Archetypes.Car.Car_Default"
  , "Archetypes.GameEvent.GameEvent_Season:CarArchetype"
  , "Archetypes.SpecialPickups.SpecialPickup_Rugby"
  , "ProjectX.NetModeReplicator"
  , "TAGame.Ball_Breakout_TA"
  , "TAGame.Ball_God_TA"
  , "TAGame.Ball_Haunted_TA"
  , "TAGame.Ball_TA"
  , "TAGame.CameraSettingsActor_TA"
  , "TAGame.Car_Season_TA"
  , "TAGame.Car_TA"
  , "TAGame.CarComponent_Boost_TA"
  , "TAGame.CarComponent_Dodge_TA"
  , "TAGame.CarComponent_DoubleJump_TA"
  , "TAGame.CarComponent_FlipCar_TA"
  , "TAGame.CarComponent_Jump_TA"
  , "TAGame.Default__CameraSettingsActor_TA"
  , "TAGame.Default__PRI_TA"
  , "TAGame.GameEvent_Football_TA"
  , "TAGame.GameEvent_GodBall_TA"
  , "TAGame.GameEvent_Season_TA"
  , "TAGame.GameEvent_Soccar_TA"
  , "TAGame.GameEvent_SoccarPrivate_TA"
  , "TAGame.GameEvent_SoccarSplitscreen_TA"
  , "TAGame.GRI_TA"
  , "TAGame.MaxTimeWarningData_TA"
  , "TAGame.RumblePickups_TA"
  , "TAGame.PRI_TA"
  , "TAGame.SpecialPickup_BallCarSpring_TA"
  , "TAGame.SpecialPickup_BallFreeze_TA"
  , "TAGame.SpecialPickup_BallGravity_TA"
  , "TAGame.SpecialPickup_BallLasso_TA"
  , "TAGame.SpecialPickup_BallVelcro_TA"
  , "TAGame.SpecialPickup_Batarang_TA"
  , "TAGame.SpecialPickup_BoostOverride_TA"
  , "TAGame.SpecialPickup_Football_TA"
  , "TAGame.SpecialPickup_GrapplingHook_TA"
  , "TAGame.SpecialPickup_HauntedBallBeam_TA"
  , "TAGame.SpecialPickup_HitForce_TA"
  , "TAGame.SpecialPickup_Rugby_TA"
  , "TAGame.SpecialPickup_Swapper_TA"
  , "TAGame.SpecialPickup_Tornado_TA"
  , "TAGame.Team_Soccar_TA"
  , "TheWorld:PersistentLevel.BreakOutActor_Platform_TA"
  , "TheWorld:PersistentLevel.CrowdActor_TA"
  , "TheWorld:PersistentLevel.CrowdManager_TA"
  , "TheWorld:PersistentLevel.InMapScoreboard_TA"
  , "TheWorld:PersistentLevel.VehiclePickup_Boost_TA"
  ]

rawClassesWithRotation :: [String]
rawClassesWithRotation =
  [ "Archetypes.Ball.Ball_BasketBall_Mutator"
  , "Archetypes.Ball.Ball_Basketball"
  , "Archetypes.Ball.Ball_BasketBall"
  , "Archetypes.Ball.Ball_Breakout"
  , "Archetypes.Ball.Ball_Default"
  , "Archetypes.Ball.Ball_Puck"
  , "Archetypes.Ball.CubeBall"
  , "Archetypes.Car.Car_Default"
  , "Archetypes.GameEvent.GameEvent_Season:CarArchetype"
  , "Archetypes.SpecialPickups.SpecialPickup_Rugby"
  , "TAGame.Ball_Breakout_TA"
  , "TAGame.Ball_God_TA"
  , "TAGame.Ball_Haunted_TA"
  , "TAGame.Ball_TA"
  , "TAGame.Car_Season_TA"
  , "TAGame.Car_TA"
  ]

rawObjectClasses :: [(String, String)]
rawObjectClasses =
  [ ("Archetypes.Ball.Ball_Anniversary", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_BasketBall_Mutator", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_Basketball", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_BasketBall", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_Beachball", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_Breakout", "TAGame.Ball_Breakout_TA")
  , ("Archetypes.Ball.Ball_Default", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_Football", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_God", "TAGame.Ball_God_TA")
  , ("Archetypes.Ball.Ball_Haunted", "TAGame.Ball_Haunted_TA")
  , ("Archetypes.Ball.Ball_Puck", "TAGame.Ball_TA")
  , ("Archetypes.Ball.CubeBall", "TAGame.Ball_TA")
  , ("Archetypes.Car.Car_Default", "TAGame.Car_TA")
  , ("Archetypes.CarComponents.CarComponent_Boost", "TAGame.CarComponent_Boost_TA")
  , ("Archetypes.CarComponents.CarComponent_Dodge", "TAGame.CarComponent_Dodge_TA")
  , ("Archetypes.CarComponents.CarComponent_DoubleJump", "TAGame.CarComponent_DoubleJump_TA")
  , ("Archetypes.CarComponents.CarComponent_FlipCar", "TAGame.CarComponent_FlipCar_TA")
  , ("Archetypes.CarComponents.CarComponent_Jump", "TAGame.CarComponent_Jump_TA")
  , ("Archetypes.GameEvent.GameEvent_Basketball", "TAGame.GameEvent_Soccar_TA")
  , ("Archetypes.GameEvent.GameEvent_BasketballPrivate", "TAGame.GameEvent_SoccarPrivate_TA")
  , ("Archetypes.GameEvent.GameEvent_BasketballSplitscreen", "TAGame.GameEvent_SoccarSplitscreen_TA")
  , ("Archetypes.GameEvent.GameEvent_Breakout", "TAGame.GameEvent_Soccar_TA")
  , ("Archetypes.GameEvent.GameEvent_Hockey", "TAGame.GameEvent_Soccar_TA")
  , ("Archetypes.GameEvent.GameEvent_HockeyPrivate", "TAGame.GameEvent_SoccarPrivate_TA")
  , ("Archetypes.GameEvent.GameEvent_HockeySplitscreen", "TAGame.GameEvent_SoccarSplitscreen_TA")
  , ("Archetypes.GameEvent.GameEvent_Items", "TAGame.GameEvent_Soccar_TA")
  , ("Archetypes.GameEvent.GameEvent_Season:CarArchetype", "TAGame.Car_TA")
  , ("Archetypes.GameEvent.GameEvent_Season", "TAGame.GameEvent_Season_TA")
  , ("Archetypes.GameEvent.GameEvent_Soccar", "TAGame.GameEvent_Soccar_TA")
  , ("Archetypes.GameEvent.GameEvent_SoccarLan", "TAGame.GameEvent_Soccar_TA")
  , ("Archetypes.GameEvent.GameEvent_SoccarPrivate", "TAGame.GameEvent_SoccarPrivate_TA")
  , ("Archetypes.GameEvent.GameEvent_SoccarSplitscreen", "TAGame.GameEvent_SoccarSplitscreen_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_BallFreeze", "TAGame.SpecialPickup_BallFreeze_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_BallGrapplingHook", "TAGame.SpecialPickup_GrapplingHook_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_BallLasso", "TAGame.SpecialPickup_BallLasso_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_BallSpring", "TAGame.SpecialPickup_BallCarSpring_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_BallVelcro", "TAGame.SpecialPickup_BallVelcro_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_Batarang", "TAGame.SpecialPickup_Batarang_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_BoostOverride", "TAGame.SpecialPickup_BoostOverride_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_CarSpring", "TAGame.SpecialPickup_BallCarSpring_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_Football", "TAGame.SpecialPickup_Football_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_GravityWell", "TAGame.SpecialPickup_BallGravity_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_HauntedBallBeam", "TAGame.SpecialPickup_HauntedBallBeam_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_Rugby", "TAGame.SpecialPickup_Rugby_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_StrongHit", "TAGame.SpecialPickup_HitForce_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_Swapper", "TAGame.SpecialPickup_Swapper_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_Tornado", "TAGame.SpecialPickup_Tornado_TA")
  , ("Archetypes.Teams.Team0", "TAGame.Team_Soccar_TA")
  , ("Archetypes.Teams.Team1", "TAGame.Team_Soccar_TA")
  , ("GameInfo_Basketball.GameInfo.GameInfo_Basketball:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("GameInfo_Breakout.GameInfo.GameInfo_Breakout:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("GameInfo_FootBall.GameInfo.GameInfo_FootBall:Archetype", "TAGame.GameEvent_Football_TA")
  , ("GameInfo_FootBall.GameInfo.GameInfo_FootBall:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("gameinfo_godball.GameInfo.gameinfo_godball:Archetype", "TAGame.GameEvent_GodBall_TA")
  , ("gameinfo_godball.GameInfo.gameinfo_godball:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("Gameinfo_Hockey.GameInfo.Gameinfo_Hockey:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("GameInfo_Items.GameInfo.GameInfo_Items:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("GameInfo_Season.GameInfo.GameInfo_Season:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("GameInfo_Soccar.GameInfo.GameInfo_Soccar:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("Haunted_TrainStation_P.TheWorld:PersistentLevel.HauntedBallTrapTrigger_TA_0", "TAGame.HauntedBallTrapTrigger_TA")
  , ("Haunted_TrainStation_P.TheWorld:PersistentLevel.HauntedBallTrapTrigger_TA_1", "TAGame.HauntedBallTrapTrigger_TA")
  , ("ProjectX.Default__NetModeReplicator_X", "ProjectX.NetModeReplicator")
  , ("TAGame.Default__CameraSettingsActor_TA", "TAGame.CameraSettingsActor_TA")
  , ("TAGame.Default__MaxTimeWarningData_TA", "TAGame.MaxTimeWarningData_TA")
  , ("TAGame.Default__RumblePickups_TA", "TAGame.RumblePickups_TA")
  , ("TAGame.Default__PRI_TA", "TAGame.PRI_TA")
  , ("TheWorld:PersistentLevel.BreakOutActor_Platform_TA", "TAGame.BreakOutActor_Platform_TA")
  , ("TheWorld:PersistentLevel.CrowdActor_TA", "TAGame.CrowdActor_TA")
  , ("TheWorld:PersistentLevel.CrowdManager_TA", "TAGame.CrowdManager_TA")
  , ("TheWorld:PersistentLevel.InMapScoreboard_TA", "TAGame.InMapScoreboard_TA")
  , ("TheWorld:PersistentLevel.VehiclePickup_Boost_TA", "TAGame.VehiclePickup_Boost_TA")
  ]

rawAttributeTypes :: [(String, AttributeType)]
rawAttributeTypes =
  [ ("Engine.Actor:bBlockActors", AttributeTypeBoolean)
  , ("Engine.Actor:bCollideActors", AttributeTypeBoolean)
  , ("Engine.Actor:bHidden", AttributeTypeBoolean)
  , ("Engine.Actor:DrawScale", AttributeTypeFloat)
  , ("Engine.Actor:RemoteRole", AttributeTypeEnum)
  , ("Engine.Actor:Role", AttributeTypeEnum)
  , ("Engine.GameReplicationInfo:bMatchIsOver", AttributeTypeBoolean)
  , ("Engine.GameReplicationInfo:GameClass", AttributeTypeFlaggedInt)
  , ("TAGame.Car_TA:RumblePickups", AttributeTypeFlaggedInt)
  , ("Engine.GameReplicationInfo:ServerName", AttributeTypeString)
  , ("Engine.Pawn:PlayerReplicationInfo", AttributeTypeFlaggedInt)
  , ("Engine.PlayerReplicationInfo:bBot", AttributeTypeBoolean)
  , ("Engine.PlayerReplicationInfo:bIsSpectator", AttributeTypeBoolean)
  , ("Engine.PlayerReplicationInfo:bReadyToPlay", AttributeTypeBoolean)
  , ("Engine.PlayerReplicationInfo:bTimedOut", AttributeTypeBoolean)
  , ("Engine.PlayerReplicationInfo:bWaitingPlayer", AttributeTypeBoolean)
  , ("Engine.PlayerReplicationInfo:Ping", AttributeTypeByte)
  , ("Engine.PlayerReplicationInfo:PlayerID", AttributeTypeInt)
  , ("Engine.PlayerReplicationInfo:PlayerName", AttributeTypeString)
  , ("Engine.PlayerReplicationInfo:RemoteUserData", AttributeTypeString)
  , ("Engine.PlayerReplicationInfo:Score", AttributeTypeInt)
  , ("Engine.PlayerReplicationInfo:Team", AttributeTypeFlaggedInt)
  , ("Engine.PlayerReplicationInfo:UniqueId", AttributeTypeUniqueId)
  , ("Engine.ReplicatedActor_ORS:ReplicatedOwner", AttributeTypeFlaggedInt)
  , ("Engine.TeamInfo:Score", AttributeTypeInt)
  , ("ProjectX.GRI_X:bGameStarted", AttributeTypeBoolean)
  , ("ProjectX.GRI_X:GameServerID", AttributeTypeQWord)
  , ("ProjectX.GRI_X:MatchGUID", AttributeTypeString)
  , ("ProjectX.GRI_X:ReplicatedGameMutatorIndex", AttributeTypeInt)
  , ("ProjectX.GRI_X:ReplicatedGamePlaylist", AttributeTypeInt)
  , ("ProjectX.GRI_X:Reservations", AttributeTypeReservation)
  , ("TAGame.Ball_Breakout_TA:AppliedDamage", AttributeTypeAppliedDamage)
  , ("TAGame.Ball_Breakout_TA:DamageIndex", AttributeTypeInt)
  , ("TAGame.Ball_Breakout_TA:LastTeamTouch", AttributeTypeByte)
  , ("TAGame.Ball_God_TA:TargetSpeed", AttributeTypeFloat)
  , ("TAGame.Ball_Haunted_TA:bIsBallBeamed", AttributeTypeBoolean)
  , ("TAGame.Ball_Haunted_TA:DeactivatedGoalIndex", AttributeTypeByte)
  , ("TAGame.Ball_Haunted_TA:LastTeamTouch", AttributeTypeByte)
  , ("TAGame.Ball_Haunted_TA:ReplicatedBeamBrokenValue", AttributeTypeByte)
  , ("TAGame.Ball_Haunted_TA:TotalActiveBeams", AttributeTypeByte)
  , ("TAGame.Ball_TA:GameEvent", AttributeTypeFlaggedInt)
  , ("TAGame.Ball_TA:HitTeamNum", AttributeTypeByte)
  , ("TAGame.Ball_TA:ReplicatedAddedCarBounceScale", AttributeTypeFloat)
  , ("TAGame.Ball_TA:ReplicatedBallMaxLinearSpeedScale", AttributeTypeFloat)
  , ("TAGame.Ball_TA:ReplicatedBallScale", AttributeTypeFloat)
  , ("TAGame.Ball_TA:ReplicatedExplosionData", AttributeTypeExplosion)
  , ("TAGame.Ball_TA:ReplicatedExplosionDataExtended", AttributeTypeExtendedExplosion)
  , ("TAGame.Ball_TA:ReplicatedPhysMatOverride", AttributeTypeFlaggedInt)
  , ("TAGame.Ball_TA:ReplicatedWorldBounceScale", AttributeTypeFloat)
  , ("TAGame.BreakOutActor_Platform_TA:DamageState", AttributeTypeDamageState)
  , ("TAGame.CameraSettingsActor_TA:bMouseCameraToggleEnabled", AttributeTypeBoolean)
  , ("TAGame.CameraSettingsActor_TA:bUsingBehindView", AttributeTypeBoolean)
  , ("TAGame.CameraSettingsActor_TA:bUsingSecondaryCamera", AttributeTypeBoolean)
  , ("TAGame.CameraSettingsActor_TA:bUsingSwivel", AttributeTypeBoolean)
  , ("TAGame.CameraSettingsActor_TA:CameraPitch", AttributeTypeByte)
  , ("TAGame.CameraSettingsActor_TA:CameraYaw", AttributeTypeByte)
  , ("TAGame.CameraSettingsActor_TA:PRI", AttributeTypeFlaggedInt)
  , ("TAGame.CameraSettingsActor_TA:ProfileSettings", AttributeTypeCamSettings)
  , ("TAGame.Car_TA:AddedBallForceMultiplier", AttributeTypeFloat)
  , ("TAGame.Car_TA:AddedCarForceMultiplier", AttributeTypeFloat)
  , ("TAGame.Car_TA:AttachedPickup", AttributeTypeFlaggedInt)
  , ("TAGame.Car_TA:ClubColors", AttributeTypeClubColors)
  , ("TAGame.Car_TA:ReplicatedDemolish_CustomFX", AttributeTypeCustomDemolish)
  , ("TAGame.Car_TA:ReplicatedDemolish", AttributeTypeDemolish)
  , ("TAGame.Car_TA:TeamPaint", AttributeTypeTeamPaint)
  , ("TAGame.CarComponent_Boost_TA:bNoBoost", AttributeTypeBoolean)
  , ("TAGame.CarComponent_Boost_TA:BoostModifier", AttributeTypeFloat)
  , ("TAGame.CarComponent_Boost_TA:bUnlimitedBoost", AttributeTypeBoolean)
  , ("TAGame.CarComponent_Boost_TA:RechargeDelay", AttributeTypeFloat)
  , ("TAGame.CarComponent_Boost_TA:RechargeRate", AttributeTypeFloat)
  , ("TAGame.CarComponent_Boost_TA:ReplicatedBoostAmount", AttributeTypeByte)
  , ("TAGame.CarComponent_Boost_TA:UnlimitedBoostRefCount", AttributeTypeInt)
  , ("TAGame.CarComponent_Dodge_TA:DodgeTorque", AttributeTypeLocation)
  , ("TAGame.CarComponent_FlipCar_TA:bFlipRight", AttributeTypeBoolean)
  , ("TAGame.CarComponent_FlipCar_TA:FlipCarTime", AttributeTypeFloat)
  , ("TAGame.CarComponent_TA:ReplicatedActive", AttributeTypeByte)
  , ("TAGame.CarComponent_TA:ReplicatedActivityTime", AttributeTypeFloat)
  , ("TAGame.CarComponent_TA:Vehicle", AttributeTypeFlaggedInt)
  , ("TAGame.CrowdActor_TA:GameEvent", AttributeTypeFlaggedInt)
  , ("TAGame.CrowdActor_TA:ModifiedNoise", AttributeTypeFloat)
  , ("TAGame.CrowdActor_TA:ReplicatedCountDownNumber", AttributeTypeInt)
  , ("TAGame.CrowdActor_TA:ReplicatedOneShotSound", AttributeTypeFlaggedInt)
  , ("TAGame.CrowdActor_TA:ReplicatedRoundCountDownNumber", AttributeTypeInt)
  , ("TAGame.CrowdManager_TA:GameEvent", AttributeTypeFlaggedInt)
  , ("TAGame.CrowdManager_TA:ReplicatedGlobalOneShotSound", AttributeTypeFlaggedInt)
  , ("TAGame.GameEvent_Soccar_TA:bBallHasBeenHit", AttributeTypeBoolean)
  , ("TAGame.GameEvent_Soccar_TA:bClubMatch", AttributeTypeBoolean)
  , ("TAGame.GameEvent_Soccar_TA:bOverTime", AttributeTypeBoolean)
  , ("TAGame.GameEvent_Soccar_TA:bUnlimitedTime", AttributeTypeBoolean)
  , ("TAGame.GameEvent_Soccar_TA:GameTime", AttributeTypeInt)
  , ("TAGame.GameEvent_Soccar_TA:MaxScore", AttributeTypeInt)
  , ("TAGame.GameEvent_Soccar_TA:ReplicatedMusicStinger", AttributeTypeMusicStinger)
  , ("TAGame.GameEvent_Soccar_TA:ReplicatedScoredOnTeam", AttributeTypeByte)
  , ("TAGame.GameEvent_Soccar_TA:ReplicatedServerPerformanceState", AttributeTypeByte)
  , ("TAGame.GameEvent_Soccar_TA:ReplicatedStatEvent", AttributeTypeStatEvent)
  , ("TAGame.GameEvent_Soccar_TA:RoundNum", AttributeTypeInt)
  , ("TAGame.GameEvent_Soccar_TA:SecondsRemaining", AttributeTypeInt)
  , ("TAGame.GameEvent_Soccar_TA:SeriesLength", AttributeTypeInt)
  , ("TAGame.GameEvent_Soccar_TA:SubRulesArchetype", AttributeTypeFlaggedInt)
  , ("TAGame.GameEvent_SoccarPrivate_TA:MatchSettings", AttributeTypePrivateMatchSettings)
  , ("TAGame.GameEvent_TA:bCanVoteToForfeit", AttributeTypeBoolean)
  , ("TAGame.GameEvent_TA:bHasLeaveMatchPenalty", AttributeTypeBoolean)
  , ("TAGame.GameEvent_TA:BotSkill", AttributeTypeInt)
  , ("TAGame.GameEvent_TA:GameMode", AttributeTypeGameMode)
  , ("TAGame.GameEvent_TA:MatchTypeClass", AttributeTypeFlaggedInt)
  , ("TAGame.GameEvent_TA:ReplicatedGameStateTimeRemaining", AttributeTypeInt)
  , ("TAGame.GameEvent_TA:ReplicatedRoundCountDownNumber", AttributeTypeInt)
  , ("TAGame.GameEvent_TA:ReplicatedStateIndex", AttributeTypeByte)
  , ("TAGame.GameEvent_TA:ReplicatedStateName", AttributeTypeInt)
  , ("TAGame.GameEvent_Team_TA:bForfeit", AttributeTypeBoolean)
  , ("TAGame.GameEvent_Team_TA:MaxTeamSize", AttributeTypeInt)
  , ("TAGame.GRI_TA:NewDedicatedServerIP", AttributeTypeString)
  , ("TAGame.MaxTimeWarningData_TA:EndGameEpochTime", AttributeTypeInt64)
  , ("TAGame.MaxTimeWarningData_TA:EndGameWarningEpochTime", AttributeTypeInt64)
  , ("TAGame.PRI_TA:bIsDistracted", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:bIsInSplitScreen", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:bMatchMVP", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:bOnlineLoadoutSet", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:bOnlineLoadoutsSet", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:BotProductName", AttributeTypeInt)
  , ("TAGame.PRI_TA:bReady", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:bUsingBehindView", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:bUsingItems", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:bUsingSecondaryCamera", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:CameraPitch", AttributeTypeByte)
  , ("TAGame.PRI_TA:CameraSettings", AttributeTypeCamSettings)
  , ("TAGame.PRI_TA:CameraYaw", AttributeTypeByte)
  , ("TAGame.PRI_TA:ClientLoadout", AttributeTypeLoadout)
  , ("TAGame.PRI_TA:ClientLoadoutOnline", AttributeTypeLoadoutOnline)
  , ("TAGame.PRI_TA:ClientLoadouts", AttributeTypeLoadouts)
  , ("TAGame.PRI_TA:ClientLoadoutsOnline", AttributeTypeLoadoutsOnline)
  , ("TAGame.PRI_TA:ClubID", AttributeTypeInt64)
  , ("TAGame.PRI_TA:MatchAssists", AttributeTypeInt)
  , ("TAGame.PRI_TA:MatchBreakoutDamage", AttributeTypeInt)
  , ("TAGame.PRI_TA:MatchGoals", AttributeTypeInt)
  , ("TAGame.PRI_TA:MatchSaves", AttributeTypeInt)
  , ("TAGame.PRI_TA:MatchScore", AttributeTypeInt)
  , ("TAGame.PRI_TA:MatchShots", AttributeTypeInt)
  , ("TAGame.PRI_TA:MaxTimeTillItem", AttributeTypeInt)
  , ("TAGame.PRI_TA:PartyLeader", AttributeTypePartyLeader)
  , ("TAGame.PRI_TA:PawnType", AttributeTypeByte)
  , ("TAGame.PRI_TA:PersistentCamera", AttributeTypeFlaggedInt)
  , ("TAGame.PRI_TA:PlayerHistoryKey", AttributeTypePlayerHistoryKey)
  , ("TAGame.PRI_TA:PlayerHistoryValid", AttributeTypeBoolean)
  , ("TAGame.PRI_TA:PrimaryTitle", AttributeTypeTitle)
  , ("TAGame.PRI_TA:ReplicatedGameEvent", AttributeTypeFlaggedInt)
  , ("TAGame.PRI_TA:ReplicatedWorstNetQualityBeyondLatency", AttributeTypeByte)
  , ("TAGame.PRI_TA:SecondaryTitle", AttributeTypeTitle)
  , ("TAGame.PRI_TA:SkillTier", AttributeTypeFlaggedByte)
  , ("TAGame.PRI_TA:SpectatorShortcut", AttributeTypeInt)
  , ("TAGame.PRI_TA:SteeringSensitivity", AttributeTypeFloat)
  , ("TAGame.PRI_TA:TimeTillItem", AttributeTypeInt)
  , ("TAGame.PRI_TA:Title", AttributeTypeInt)
  , ("TAGame.PRI_TA:TotalXP", AttributeTypeInt)
  , ("TAGame.RBActor_TA:bFrozen", AttributeTypeBoolean)
  , ("TAGame.RBActor_TA:bIgnoreSyncing", AttributeTypeBoolean)
  , ("TAGame.RBActor_TA:bReplayActor", AttributeTypeBoolean)
  , ("TAGame.RBActor_TA:ReplicatedRBState", AttributeTypeRigidBodyState)
  , ("TAGame.RBActor_TA:WeldedInfo", AttributeTypeWeldedInfo)
  , ("TAGame.RumblePickups_TA:AttachedPickup", AttributeTypeFlaggedInt)
  , ("TAGame.RumblePickups_TA:ConcurrentItemCount", AttributeTypeInt)
  , ("TAGame.SpecialPickup_BallFreeze_TA:RepOrigSpeed", AttributeTypeFloat)
  , ("TAGame.SpecialPickup_BallVelcro_TA:AttachTime", AttributeTypeFloat)
  , ("TAGame.SpecialPickup_BallVelcro_TA:bBroken", AttributeTypeBoolean)
  , ("TAGame.SpecialPickup_BallVelcro_TA:bHit", AttributeTypeBoolean)
  , ("TAGame.SpecialPickup_BallVelcro_TA:BreakTime", AttributeTypeFloat)
  , ("TAGame.SpecialPickup_Football_TA:WeldedBall", AttributeTypeFlaggedInt)
  , ("TAGame.SpecialPickup_Rugby_TA:bBallWelded", AttributeTypeBoolean)
  , ("TAGame.SpecialPickup_Targeted_TA:Targeted", AttributeTypeFlaggedInt)
  , ("TAGame.Team_Soccar_TA:GameScore", AttributeTypeInt)
  , ("TAGame.Team_TA:ClubColors", AttributeTypeClubColors)
  , ("TAGame.Team_TA:ClubID", AttributeTypeInt64)
  , ("TAGame.Team_TA:CustomTeamName", AttributeTypeString)
  , ("TAGame.Team_TA:Difficulty", AttributeTypeInt)
  , ("TAGame.Team_TA:GameEvent", AttributeTypeFlaggedInt)
  , ("TAGame.Team_TA:LogoData", AttributeTypeFlaggedInt)
  , ("TAGame.Vehicle_TA:bDriving", AttributeTypeBoolean)
  , ("TAGame.Vehicle_TA:bReplicatedHandbrake", AttributeTypeBoolean)
  , ("TAGame.Vehicle_TA:ReplicatedSteer", AttributeTypeByte)
  , ("TAGame.Vehicle_TA:ReplicatedThrottle", AttributeTypeByte)
  , ("TAGame.VehiclePickup_TA:bNoPickup", AttributeTypeBoolean)
  , ("TAGame.VehiclePickup_TA:NewReplicatedPickupData", AttributeTypePickupNew)
  , ("TAGame.VehiclePickup_TA:ReplicatedPickupData", AttributeTypePickup)
  ]
