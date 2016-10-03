module Rattletrap.Data where

rawParentClasses :: [(String, String)]
rawParentClasses = [] -- TODO

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
  ["TAGame.Ball_TA", "TAGame.Car_Season_TA", "TAGame.Car_TA"]

rawObjectClasses :: [(String, String)]
rawObjectClasses =
  [ ("Archetypes.Ball.Ball_Basketball", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_Default", "TAGame.Ball_TA")
  , ("Archetypes.Ball.Ball_Puck", "TAGame.Ball_TA")
  , ("Archetypes.Ball.CubeBall", "TAGame.Ball_TA")
  , ("Archetypes.Car.Car_Default", "TAGame.Car_TA")
  , ( "Archetypes.CarComponents.CarComponent_Boost"
    , "TAGame.CarComponent_Boost_TA")
  , ( "Archetypes.CarComponents.CarComponent_Dodge"
    , "TAGame.CarComponent_Dodge_TA")
  , ( "Archetypes.CarComponents.CarComponent_DoubleJump"
    , "TAGame.CarComponent_DoubleJump_TA")
  , ( "Archetypes.CarComponents.CarComponent_FlipCar"
    , "TAGame.CarComponent_FlipCar_TA")
  , ( "Archetypes.CarComponents.CarComponent_Jump"
    , "TAGame.CarComponent_Jump_TA")
  , ("Archetypes.GameEvent.GameEvent_Basketball", "TAGame.GameEvent_Soccar_TA")
  , ( "Archetypes.GameEvent.GameEvent_BasketballPrivate"
    , "TAGame.GameEvent_SoccarPrivate_TA")
  , ( "Archetypes.GameEvent.GameEvent_BasketballSplitscreen"
    , "TAGame.GameEvent_SoccarSplitscreen_TA")
  , ( "Archetypes.GameEvent.GameEvent_HockeyPrivate"
    , "TAGame.GameEvent_SoccarPrivate_TA")
  , ( "Archetypes.GameEvent.GameEvent_HockeySplitscreen"
    , "TAGame.GameEvent_SoccarSplitscreen_TA")
  , ("Archetypes.GameEvent.GameEvent_Items", "TAGame.GameEvent_Soccar_TA")
  , ("Archetypes.GameEvent.GameEvent_Season:CarArchetype", "TAGame.Car_TA")
  , ("Archetypes.GameEvent.GameEvent_Season", "TAGame.GameEvent_Season_TA")
  , ("Archetypes.GameEvent.GameEvent_Soccar", "TAGame.GameEvent_Soccar_TA")
  , ( "Archetypes.GameEvent.GameEvent_SoccarPrivate"
    , "TAGame.GameEvent_SoccarPrivate_TA")
  , ( "Archetypes.GameEvent.GameEvent_SoccarSplitscreen"
    , "TAGame.GameEvent_SoccarSplitscreen_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_BallFreeze"
    , "TAGame.SpecialPickup_BallFreeze_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_BallGrapplingHook"
    , "TAGame.SpecialPickup_GrapplingHook_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_BallLasso"
    , "TAGame.SpecialPickup_BallLasso_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_BallSpring"
    , "TAGame.SpecialPickup_BallCarSpring_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_BallVelcro"
    , "TAGame.SpecialPickup_BallVelcro_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_Batarang"
    , "TAGame.SpecialPickup_Batarang_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_BoostOverride"
    , "TAGame.SpecialPickup_BoostOverride_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_CarSpring"
    , "TAGame.SpecialPickup_BallCarSpring_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_GravityWell"
    , "TAGame.SpecialPickup_BallGravity_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_StrongHit"
    , "TAGame.SpecialPickup_HitForce_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_Swapper"
    , "TAGame.SpecialPickup_Swapper_TA")
  , ( "Archetypes.SpecialPickups.SpecialPickup_Tornado"
    , "TAGame.SpecialPickup_Tornado_TA")
  , ("Archetypes.Teams.Team0", "TAGame.Team_Soccar_TA")
  , ("Archetypes.Teams.Team1", "TAGame.Team_Soccar_TA")
  , ( "GameInfo_Basketball.GameInfo.GameInfo_Basketball:GameReplicationInfoArchetype"
    , "TAGame.GRI_TA")
  , ( "Gameinfo_Hockey.GameInfo.Gameinfo_Hockey:GameReplicationInfoArchetype"
    , "TAGame.GRI_TA")
  , ( "GameInfo_Items.GameInfo.GameInfo_Items:GameReplicationInfoArchetype"
    , "TAGame.GRI_TA")
  , ( "GameInfo_Season.GameInfo.GameInfo_Season:GameReplicationInfoArchetype"
    , "TAGame.GRI_TA")
  , ( "GameInfo_Soccar.GameInfo.GameInfo_Soccar:GameReplicationInfoArchetype"
    , "TAGame.GRI_TA")
  , ("TAGame.CameraSettingsActor_TA:PRI", "TAGame.CameraSettingsActor_TA")
  , ("TAGame.Default__CameraSettingsActor_TA", "TAGame.CameraSettingsActor_TA")
  , ("TAGame.Default__PRI_TA", "TAGame.PRI_TA")
  , ("TAGame.Default__VoteActor_TA", "TAGame.VoteActor_TA")
  , ("TheWorld:PersistentLevel.CrowdActor_TA", "TAGame.CrowdActor_TA")
  , ("TheWorld:PersistentLevel.CrowdManager_TA", "TAGame.CrowdManager_TA")
  , ("TheWorld:PersistentLevel.InMapScoreboard_TA", "TAGame.InMapScoreboard_TA")
  , ( "TheWorld:PersistentLevel.VehiclePickup_Boost_TA"
    , "TAGame.VehiclePickup_Boost_TA")
  ]
