module Rattletrap.Data where

import Rattletrap.AttributeValueType

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
  , ("TAGame.GameEvent_Season_TA", "TAGame.GameEvent_Soccar_TA")
  , ("TAGame.GameEvent_Soccar_TA", "TAGame.GameEvent_Team_TA")
  , ("TAGame.GameEvent_SoccarPrivate_TA", "TAGame.GameEvent_Soccar_TA")
  , ("TAGame.GameEvent_SoccarSplitscreen_TA", "TAGame.GameEvent_SoccarPrivate_TA")
  , ("TAGame.GameEvent_TA", "Engine.ReplicationInfo")
  , ("TAGame.GameEvent_Team_TA", "TAGame.GameEvent_TA")
  , ("TAGame.GRI_TA", "ProjectX.GRI_X")
  , ("TAGame.InMapScoreboard_TA", "Engine.Actor")
  , ("TAGame.PRI_TA", "ProjectX.PRI_X")
  , ("TAGame.RBActor_TA", "ProjectX.Pawn_X")
  , ("TAGame.SpecialPickup_BallCarSpring_TA", "TAGame.SpecialPickup_Spring_TA")
  , ("TAGame.SpecialPickup_BallFreeze_TA", "TAGame.SpecialPickup_Targeted_TA")
  , ("TAGame.SpecialPickup_BallGravity_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_BallLasso_TA", "TAGame.SpecialPickup_GrapplingHook_TA")
  , ("TAGame.SpecialPickup_BallVelcro_TA", "TAGame.SpecialPickup_TA")
  , ("TAGame.SpecialPickup_Batarang_TA", "TAGame.SpecialPickup_BallLasso_TA")
  , ("TAGame.SpecialPickup_BoostOverride_TA", "TAGame.SpecialPickup_Targeted_TA")
  , ("TAGame.SpecialPickup_GrapplingHook_TA", "TAGame.SpecialPickup_Targeted_TA")
  , ("TAGame.SpecialPickup_HitForce_TA", "TAGame.SpecialPickup_TA")
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
  , ("TAGame.VoteActor_TA", "Engine.Actor")
  ]

rawClassesWithLocation :: [String]
rawClassesWithLocation =
  [ "TAGame.Ball_TA"
  , "TAGame.CameraSettingsActor_TA"
  , "TAGame.Car_Season_TA"
  , "TAGame.Car_TA"
  , "TAGame.CarComponent_Boost_TA"
  , "TAGame.CarComponent_Dodge_TA"
  , "TAGame.CarComponent_DoubleJump_TA"
  , "TAGame.CarComponent_FlipCar_TA"
  , "TAGame.CarComponent_Jump_TA"
  , "TAGame.GameEvent_Season_TA"
  , "TAGame.GameEvent_Soccar_TA"
  , "TAGame.GameEvent_SoccarPrivate_TA"
  , "TAGame.GameEvent_SoccarSplitscreen_TA"
  , "TAGame.GRI_TA"
  , "TAGame.PRI_TA"
  , "TAGame.SpecialPickup_BallCarSpring_TA"
  , "TAGame.SpecialPickup_BallFreeze_TA"
  , "TAGame.SpecialPickup_BallGravity_TA"
  , "TAGame.SpecialPickup_BallGravity_TA"
  , "TAGame.SpecialPickup_BallLasso_TA"
  , "TAGame.SpecialPickup_BallVelcro_TA"
  , "TAGame.SpecialPickup_Batarang_TA"
  , "TAGame.SpecialPickup_BoostOverride_TA"
  , "TAGame.SpecialPickup_GrapplingHook_TA"
  , "TAGame.SpecialPickup_HitForce_TA"
  , "TAGame.SpecialPickup_Swapper_TA"
  , "TAGame.SpecialPickup_Tornado_TA"
  , "TAGame.Team_Soccar_TA"
  ]

rawClassesWithRotation :: [String]
rawClassesWithRotation =
  [ "TAGame.Ball_TA"
  , "TAGame.Car_Season_TA"
  , "TAGame.Car_TA"
  ]

rawObjectClasses :: [(String, String)]
rawObjectClasses =
  [ ("Archetypes.Ball.Ball_Basketball", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_Default", "TAGame.Ball_TA")
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
  , ("Archetypes.GameEvent.GameEvent_HockeyPrivate", "TAGame.GameEvent_SoccarPrivate_TA")
  , ("Archetypes.GameEvent.GameEvent_HockeySplitscreen", "TAGame.GameEvent_SoccarSplitscreen_TA")
  , ("Archetypes.GameEvent.GameEvent_Items", "TAGame.GameEvent_Soccar_TA")
  , ("Archetypes.GameEvent.GameEvent_Season:CarArchetype", "TAGame.Car_TA")
  , ("Archetypes.GameEvent.GameEvent_Season", "TAGame.GameEvent_Season_TA")
  , ("Archetypes.GameEvent.GameEvent_Soccar", "TAGame.GameEvent_Soccar_TA")
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
  , ("Archetypes.SpecialPickups.SpecialPickup_GravityWell", "TAGame.SpecialPickup_BallGravity_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_StrongHit", "TAGame.SpecialPickup_HitForce_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_Swapper", "TAGame.SpecialPickup_Swapper_TA")
  , ("Archetypes.SpecialPickups.SpecialPickup_Tornado", "TAGame.SpecialPickup_Tornado_TA")
  , ("Archetypes.Teams.Team0", "TAGame.Team_Soccar_TA")
  , ("Archetypes.Teams.Team1", "TAGame.Team_Soccar_TA")
  , ("GameInfo_Basketball.GameInfo.GameInfo_Basketball:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("Gameinfo_Hockey.GameInfo.Gameinfo_Hockey:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("GameInfo_Items.GameInfo.GameInfo_Items:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("GameInfo_Season.GameInfo.GameInfo_Season:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("GameInfo_Soccar.GameInfo.GameInfo_Soccar:GameReplicationInfoArchetype", "TAGame.GRI_TA")
  , ("TAGame.Default__CameraSettingsActor_TA", "TAGame.CameraSettingsActor_TA")
  , ("TAGame.Default__PRI_TA", "TAGame.PRI_TA")
  , ("TAGame.Default__VoteActor_TA", "TAGame.VoteActor_TA")
  , ("TheWorld:PersistentLevel.CrowdActor_TA", "TAGame.CrowdActor_TA")
  , ("TheWorld:PersistentLevel.CrowdManager_TA", "TAGame.CrowdManager_TA")
  , ("TheWorld:PersistentLevel.InMapScoreboard_TA", "TAGame.InMapScoreboard_TA")
  , ("TheWorld:PersistentLevel.VehiclePickup_Boost_TA", "TAGame.VehiclePickup_Boost_TA")
  ]

rawAttributeValueTypes :: [(String, AttributeValueType)]
rawAttributeValueTypes =
  [ ("Engine.Actor:bBlockActors", AVBoolean)
  , ("Engine.Actor:DrawScale", AVFloat)
  , ("Engine.Actor:Role", AVEnum)
  , ("Engine.Actor:bCollideActors", AVBoolean)
  , ("Engine.Actor:bHidden", AVBoolean)
  , ("Engine.GameReplicationInfo:GameClass", AVFlaggedInt)
  , ("Engine.GameReplicationInfo:ServerName", AVString)
  , ("Engine.GameReplicationInfo:bMatchIsOver", AVBoolean)
  , ("Engine.Pawn:PlayerReplicationInfo", AVFlaggedInt)
  , ("Engine.PlayerReplicationInfo:Ping", AVByte)
  , ("Engine.PlayerReplicationInfo:PlayerID", AVInt)
  , ("Engine.PlayerReplicationInfo:PlayerName", AVString)
  , ("Engine.PlayerReplicationInfo:RemoteUserData", AVString)
  , ("Engine.PlayerReplicationInfo:Score", AVInt)
  , ("Engine.PlayerReplicationInfo:Team", AVFlaggedInt)
  , ("Engine.PlayerReplicationInfo:UniqueId", AVUniqueId)
  , ("Engine.PlayerReplicationInfo:bBot", AVBoolean)
  , ("Engine.PlayerReplicationInfo:bIsSpectator", AVBoolean)
  , ("Engine.PlayerReplicationInfo:bReadyToPlay", AVBoolean)
  , ("Engine.PlayerReplicationInfo:bWaitingPlayer", AVBoolean)
  , ("Engine.TeamInfo:Score", AVInt)
  , ("ProjectX.GRI_X:GameServerID", AVQWord)
  , ("ProjectX.GRI_X:ReplicatedGameMutatorIndex", AVInt)
  , ("ProjectX.GRI_X:ReplicatedGamePlaylist", AVInt)
  , ("ProjectX.GRI_X:Reservations", AVReservation)
  , ("ProjectX.GRI_X:bGameStarted", AVBoolean)
  , ("TAGame.Ball_TA:GameEvent", AVFlaggedInt)
  , ("TAGame.Ball_TA:HitTeamNum", AVByte)
  , ("TAGame.Ball_TA:ReplicatedAddedCarBounceScale", AVFloat)
  , ("TAGame.Ball_TA:ReplicatedBallMaxLinearSpeedScale", AVFloat)
  , ("TAGame.Ball_TA:ReplicatedBallScale", AVFloat)
  , ("TAGame.Ball_TA:ReplicatedExplosionData", AVExplosion)
  , ("TAGame.Ball_TA:ReplicatedWorldBounceScale", AVFloat)
  , ("TAGame.CameraSettingsActor_TA:CameraPitch", AVByte)
  , ("TAGame.CameraSettingsActor_TA:CameraYaw", AVByte)
  , ("TAGame.CameraSettingsActor_TA:PRI", AVFlaggedInt)
  , ("TAGame.CameraSettingsActor_TA:ProfileSettings", AVCamSettings)
  , ("TAGame.CameraSettingsActor_TA:bUsingBehindView", AVBoolean)
  , ("TAGame.CameraSettingsActor_TA:bUsingSecondaryCamera", AVBoolean)
  , ("TAGame.CarComponent_Boost_TA:BoostModifier", AVFloat)
  , ("TAGame.CarComponent_Boost_TA:RechargeDelay", AVFloat)
  , ("TAGame.CarComponent_Boost_TA:RechargeRate", AVFloat)
  , ("TAGame.CarComponent_Boost_TA:ReplicatedBoostAmount", AVByte)
  , ("TAGame.CarComponent_Boost_TA:UnlimitedBoostRefCount", AVInt)
  , ("TAGame.CarComponent_Boost_TA:bNoBoost", AVBoolean)
  , ("TAGame.CarComponent_Boost_TA:bUnlimitedBoost", AVBoolean)
  , ("TAGame.CarComponent_Dodge_TA:DodgeTorque", AVLocation)
  , ("TAGame.CarComponent_FlipCar_TA:FlipCarTime", AVFloat)
  , ("TAGame.CarComponent_FlipCar_TA:bFlipRight", AVBoolean)
  , ("TAGame.CarComponent_TA:ReplicatedActive", AVByte)
  , ("TAGame.CarComponent_TA:ReplicatedActivityTime", AVFloat)
  , ("TAGame.CarComponent_TA:Vehicle", AVFlaggedInt)
  , ("TAGame.Car_TA:AddedBallForceMultiplier", AVFloat)
  , ("TAGame.Car_TA:AddedCarForceMultiplier", AVFloat)
  , ("TAGame.Car_TA:AttachedPickup", AVFlaggedInt)
  , ("TAGame.Car_TA:ReplicatedDemolish", AVDemolish)
  , ("TAGame.Car_TA:TeamPaint", AVTeamPaint)
  , ("TAGame.CrowdActor_TA:GameEvent", AVFlaggedInt)
  , ("TAGame.CrowdActor_TA:ModifiedNoise", AVFloat)
  , ("TAGame.CrowdActor_TA:ReplicatedCountDownNumber", AVInt)
  , ("TAGame.CrowdActor_TA:ReplicatedOneShotSound", AVFlaggedInt)
  , ("TAGame.CrowdActor_TA:ReplicatedRoundCountDownNumber", AVInt)
  , ("TAGame.CrowdManager_TA:GameEvent", AVFlaggedInt)
  , ("TAGame.CrowdManager_TA:ReplicatedGlobalOneShotSound", AVFlaggedInt)
  , ("TAGame.GRI_TA:NewDedicatedServerIP", AVString)
  , ("TAGame.GameEvent_SoccarPrivate_TA:MatchSettings", AVPrivateMatchSettings)
  , ("TAGame.GameEvent_Soccar_TA:ReplicatedMusicStinger", AVMusicStinger)
  , ("TAGame.GameEvent_Soccar_TA:ReplicatedScoredOnTeam", AVByte)
  , ("TAGame.GameEvent_Soccar_TA:RoundNum", AVInt)
  , ("TAGame.GameEvent_Soccar_TA:SecondsRemaining", AVInt)
  , ("TAGame.GameEvent_Soccar_TA:SubRulesArchetype", AVFlaggedInt)
  , ("TAGame.GameEvent_Soccar_TA:bBallHasBeenHit", AVBoolean)
  , ("TAGame.GameEvent_Soccar_TA:bOverTime", AVBoolean)
  , ("TAGame.GameEvent_TA:BotSkill", AVInt)
  , ("TAGame.GameEvent_TA:GameMode", AVGameMode)
  , ("TAGame.GameEvent_TA:MatchTypeClass", AVFlaggedInt)
  , ("TAGame.GameEvent_TA:ReplicatedGameStateTimeRemaining", AVInt)
  , ("TAGame.GameEvent_TA:ReplicatedStateIndex", AVByte)
  , ("TAGame.GameEvent_TA:ReplicatedStateName", AVInt)
  , ("TAGame.GameEvent_TA:bCanVoteToForfeit", AVBoolean)
  , ("TAGame.GameEvent_TA:bHasLeaveMatchPenalty", AVBoolean)
  , ("TAGame.GameEvent_Team_TA:MaxTeamSize", AVInt)
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
  , ("TAGame.PRI_TA:bIsInSplitScreen", AVBoolean)
  , ("TAGame.PRI_TA:bMatchMVP", AVBoolean)
  , ("TAGame.PRI_TA:bOnlineLoadoutSet", AVBoolean)
  , ("TAGame.PRI_TA:bOnlineLoadoutsSet", AVBoolean)
  , ("TAGame.PRI_TA:bReady", AVBoolean)
  , ("TAGame.PRI_TA:bUsingBehindView", AVBoolean)
  , ("TAGame.PRI_TA:bUsingSecondaryCamera", AVBoolean)
  , ("TAGame.RBActor_TA:ReplicatedRBState", AVRigidBodyState)
  , ("TAGame.RBActor_TA:WeldedInfo", AVWeldedInfo)
  , ("TAGame.RBActor_TA:bFrozen", AVBoolean)
  , ("TAGame.RBActor_TA:bIgnoreSyncing", AVBoolean)
  , ("TAGame.RBActor_TA:bReplayActor", AVBoolean)
  , ("TAGame.SpecialPickup_BallFreeze_TA:RepOrigSpeed", AVFloat)
  , ("TAGame.SpecialPickup_BallVelcro_TA:AttachTime", AVFloat)
  , ("TAGame.SpecialPickup_BallVelcro_TA:BreakTime", AVFloat)
  , ("TAGame.SpecialPickup_BallVelcro_TA:bBroken", AVBoolean)
  , ("TAGame.SpecialPickup_BallVelcro_TA:bHit", AVBoolean)
  , ("TAGame.SpecialPickup_Targeted_TA:Targeted", AVFlaggedInt)
  , ("TAGame.Team_Soccar_TA:GameScore", AVInt)
  , ("TAGame.Team_TA:CustomTeamName", AVString)
  , ("TAGame.Team_TA:GameEvent", AVFlaggedInt)
  , ("TAGame.Team_TA:LogoData", AVFlaggedInt)
  , ("TAGame.VehiclePickup_TA:ReplicatedPickupData", AVPickup)
  , ("TAGame.Vehicle_TA:ReplicatedSteer", AVByte)
  , ("TAGame.Vehicle_TA:ReplicatedThrottle", AVByte)
  , ("TAGame.Vehicle_TA:bDriving", AVBoolean)
  , ("TAGame.Vehicle_TA:bReplicatedHandbrake", AVBoolean)
  ]

rawCrc32Table :: Integral a => [a]
rawCrc32Table =
  [ 0x00000000, 0x04c11db7, 0x09823b6e, 0x0d4326d9
  , 0x130476dc, 0x17c56b6b, 0x1a864db2, 0x1e475005
  , 0x2608edb8, 0x22c9f00f, 0x2f8ad6d6, 0x2b4bcb61
  , 0x350c9b64, 0x31cd86d3, 0x3c8ea00a, 0x384fbdbd
  , 0x4c11db70, 0x48d0c6c7, 0x4593e01e, 0x4152fda9
  , 0x5f15adac, 0x5bd4b01b, 0x569796c2, 0x52568b75
  , 0x6a1936c8, 0x6ed82b7f, 0x639b0da6, 0x675a1011
  , 0x791d4014, 0x7ddc5da3, 0x709f7b7a, 0x745e66cd
  , 0x9823b6e0, 0x9ce2ab57, 0x91a18d8e, 0x95609039
  , 0x8b27c03c, 0x8fe6dd8b, 0x82a5fb52, 0x8664e6e5
  , 0xbe2b5b58, 0xbaea46ef, 0xb7a96036, 0xb3687d81
  , 0xad2f2d84, 0xa9ee3033, 0xa4ad16ea, 0xa06c0b5d
  , 0xd4326d90, 0xd0f37027, 0xddb056fe, 0xd9714b49
  , 0xc7361b4c, 0xc3f706fb, 0xceb42022, 0xca753d95
  , 0xf23a8028, 0xf6fb9d9f, 0xfbb8bb46, 0xff79a6f1
  , 0xe13ef6f4, 0xe5ffeb43, 0xe8bccd9a, 0xec7dd02d
  , 0x34867077, 0x30476dc0, 0x3d044b19, 0x39c556ae
  , 0x278206ab, 0x23431b1c, 0x2e003dc5, 0x2ac12072
  , 0x128e9dcf, 0x164f8078, 0x1b0ca6a1, 0x1fcdbb16
  , 0x018aeb13, 0x054bf6a4, 0x0808d07d, 0x0cc9cdca
  , 0x7897ab07, 0x7c56b6b0, 0x71159069, 0x75d48dde
  , 0x6b93dddb, 0x6f52c06c, 0x6211e6b5, 0x66d0fb02
  , 0x5e9f46bf, 0x5a5e5b08, 0x571d7dd1, 0x53dc6066
  , 0x4d9b3063, 0x495a2dd4, 0x44190b0d, 0x40d816ba
  , 0xaca5c697, 0xa864db20, 0xa527fdf9, 0xa1e6e04e
  , 0xbfa1b04b, 0xbb60adfc, 0xb6238b25, 0xb2e29692
  , 0x8aad2b2f, 0x8e6c3698, 0x832f1041, 0x87ee0df6
  , 0x99a95df3, 0x9d684044, 0x902b669d, 0x94ea7b2a
  , 0xe0b41de7, 0xe4750050, 0xe9362689, 0xedf73b3e
  , 0xf3b06b3b, 0xf771768c, 0xfa325055, 0xfef34de2
  , 0xc6bcf05f, 0xc27dede8, 0xcf3ecb31, 0xcbffd686
  , 0xd5b88683, 0xd1799b34, 0xdc3abded, 0xd8fba05a
  , 0x690ce0ee, 0x6dcdfd59, 0x608edb80, 0x644fc637
  , 0x7a089632, 0x7ec98b85, 0x738aad5c, 0x774bb0eb
  , 0x4f040d56, 0x4bc510e1, 0x46863638, 0x42472b8f
  , 0x5c007b8a, 0x58c1663d, 0x558240e4, 0x51435d53
  , 0x251d3b9e, 0x21dc2629, 0x2c9f00f0, 0x285e1d47
  , 0x36194d42, 0x32d850f5, 0x3f9b762c, 0x3b5a6b9b
  , 0x0315d626, 0x07d4cb91, 0x0a97ed48, 0x0e56f0ff
  , 0x1011a0fa, 0x14d0bd4d, 0x19939b94, 0x1d528623
  , 0xf12f560e, 0xf5ee4bb9, 0xf8ad6d60, 0xfc6c70d7
  , 0xe22b20d2, 0xe6ea3d65, 0xeba91bbc, 0xef68060b
  , 0xd727bbb6, 0xd3e6a601, 0xdea580d8, 0xda649d6f
  , 0xc423cd6a, 0xc0e2d0dd, 0xcda1f604, 0xc960ebb3
  , 0xbd3e8d7e, 0xb9ff90c9, 0xb4bcb610, 0xb07daba7
  , 0xae3afba2, 0xaafbe615, 0xa7b8c0cc, 0xa379dd7b
  , 0x9b3660c6, 0x9ff77d71, 0x92b45ba8, 0x9675461f
  , 0x8832161a, 0x8cf30bad, 0x81b02d74, 0x857130c3
  , 0x5d8a9099, 0x594b8d2e, 0x5408abf7, 0x50c9b640
  , 0x4e8ee645, 0x4a4ffbf2, 0x470cdd2b, 0x43cdc09c
  , 0x7b827d21, 0x7f436096, 0x7200464f, 0x76c15bf8
  , 0x68860bfd, 0x6c47164a, 0x61043093, 0x65c52d24
  , 0x119b4be9, 0x155a565e, 0x18197087, 0x1cd86d30
  , 0x029f3d35, 0x065e2082, 0x0b1d065b, 0x0fdc1bec
  , 0x3793a651, 0x3352bbe6, 0x3e119d3f, 0x3ad08088
  , 0x2497d08d, 0x2056cd3a, 0x2d15ebe3, 0x29d4f654
  , 0xc5a92679, 0xc1683bce, 0xcc2b1d17, 0xc8ea00a0
  , 0xd6ad50a5, 0xd26c4d12, 0xdf2f6bcb, 0xdbee767c
  , 0xe3a1cbc1, 0xe760d676, 0xea23f0af, 0xeee2ed18
  , 0xf0a5bd1d, 0xf464a0aa, 0xf9278673, 0xfde69bc4
  , 0x89b8fd09, 0x8d79e0be, 0x803ac667, 0x84fbdbd0
  , 0x9abc8bd5, 0x9e7d9662, 0x933eb0bb, 0x97ffad0c
  , 0xafb010b1, 0xab710d06, 0xa6322bdf, 0xa2f33668
  , 0xbcb4666d, 0xb8757bda, 0xb5365d03, 0xb1f740b4
  ]
