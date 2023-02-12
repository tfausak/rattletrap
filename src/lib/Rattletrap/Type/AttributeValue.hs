module Rattletrap.Type.AttributeValue where

import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Data as Data
import qualified Rattletrap.Exception.UnknownAttribute as UnknownAttribute
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.AppliedDamage as AppliedDamage
import qualified Rattletrap.Type.Attribute.Boolean as Boolean
import qualified Rattletrap.Type.Attribute.Byte as Byte
import qualified Rattletrap.Type.Attribute.CamSettings as CamSettings
import qualified Rattletrap.Type.Attribute.ClubColors as ClubColors
import qualified Rattletrap.Type.Attribute.CustomDemolish as CustomDemolish
import qualified Rattletrap.Type.Attribute.DamageState as DamageState
import qualified Rattletrap.Type.Attribute.Demolish as Demolish
import qualified Rattletrap.Type.Attribute.Enum as Enum
import qualified Rattletrap.Type.Attribute.Explosion as Explosion
import qualified Rattletrap.Type.Attribute.ExtendedExplosion as ExtendedExplosion
import qualified Rattletrap.Type.Attribute.FlaggedByte as FlaggedByte
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import qualified Rattletrap.Type.Attribute.Float as Float
import qualified Rattletrap.Type.Attribute.GameMode as GameMode
import qualified Rattletrap.Type.Attribute.GameServer as GameServer
import qualified Rattletrap.Type.Attribute.Int as Int
import qualified Rattletrap.Type.Attribute.Int64 as Int64
import qualified Rattletrap.Type.Attribute.Loadout as Loadout
import qualified Rattletrap.Type.Attribute.LoadoutOnline as LoadoutOnline
import qualified Rattletrap.Type.Attribute.Loadouts as Loadouts
import qualified Rattletrap.Type.Attribute.LoadoutsOnline as LoadoutsOnline
import qualified Rattletrap.Type.Attribute.Location as Location
import qualified Rattletrap.Type.Attribute.MusicStinger as MusicStinger
import qualified Rattletrap.Type.Attribute.PartyLeader as PartyLeader
import qualified Rattletrap.Type.Attribute.Pickup as Pickup
import qualified Rattletrap.Type.Attribute.PickupInfo as PickupInfo
import qualified Rattletrap.Type.Attribute.PickupNew as PickupNew
import qualified Rattletrap.Type.Attribute.PlayerHistoryKey as PlayerHistoryKey
import qualified Rattletrap.Type.Attribute.PrivateMatchSettings as PrivateMatchSettings
import qualified Rattletrap.Type.Attribute.QWord as QWord
import qualified Rattletrap.Type.Attribute.RepStatTitle as RepStatTitle
import qualified Rattletrap.Type.Attribute.Reservation as Reservation
import qualified Rattletrap.Type.Attribute.RigidBodyState as RigidBodyState
import qualified Rattletrap.Type.Attribute.Rotation as Rotation
import qualified Rattletrap.Type.Attribute.StatEvent as StatEvent
import qualified Rattletrap.Type.Attribute.String as String
import qualified Rattletrap.Type.Attribute.TeamPaint as TeamPaint
import qualified Rattletrap.Type.Attribute.Title as Title
import qualified Rattletrap.Type.Attribute.UniqueId as UniqueId
import qualified Rattletrap.Type.Attribute.WeldedInfo as WeldedInfo
import qualified Rattletrap.Type.AttributeType as AttributeType
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data AttributeValue
  = AppliedDamage AppliedDamage.AppliedDamage
  | Boolean Boolean.Boolean
  | Byte Byte.Byte
  | CamSettings CamSettings.CamSettings
  | ClubColors ClubColors.ClubColors
  | CustomDemolish CustomDemolish.CustomDemolish
  | DamageState DamageState.DamageState
  | Demolish Demolish.Demolish
  | Enum Enum.Enum
  | Explosion Explosion.Explosion
  | ExtendedExplosion ExtendedExplosion.ExtendedExplosion
  | FlaggedInt FlaggedInt.FlaggedInt
  | FlaggedByte FlaggedByte.FlaggedByte
  | Float Float.Float
  | GameMode GameMode.GameMode
  | GameServer GameServer.GameServer
  | Int Int.Int
  | Int64 Int64.Int64
  | Loadout Loadout.Loadout
  | LoadoutOnline LoadoutOnline.LoadoutOnline
  | Loadouts Loadouts.Loadouts
  | LoadoutsOnline LoadoutsOnline.LoadoutsOnline
  | Location Location.Location
  | MusicStinger MusicStinger.MusicStinger
  | PartyLeader PartyLeader.PartyLeader
  | Pickup Pickup.Pickup
  | PickupInfo PickupInfo.PickupInfo
  | PickupNew PickupNew.PickupNew
  | PlayerHistoryKey PlayerHistoryKey.PlayerHistoryKey
  | PrivateMatchSettings PrivateMatchSettings.PrivateMatchSettings
  | QWord QWord.QWord
  | RepStatTitle RepStatTitle.RepStatTitle
  | Reservation Reservation.Reservation
  | RigidBodyState RigidBodyState.RigidBodyState
  | Rotation Rotation.Rotation
  | StatEvent StatEvent.StatEvent
  | String String.String
  | TeamPaint TeamPaint.TeamPaint
  | Title Title.Title
  | UniqueId UniqueId.UniqueId
  | WeldedInfo WeldedInfo.WeldedInfo
  deriving (Eq, Show)

instance Json.FromJSON AttributeValue where
  parseJSON = Json.withObject "AttributeValue" $ \object ->
    Foldable.asum
      [ fmap AppliedDamage $ Json.required object "applied_damage",
        fmap Boolean $ Json.required object "boolean",
        fmap Byte $ Json.required object "byte",
        fmap CamSettings $ Json.required object "cam_settings",
        fmap ClubColors $ Json.required object "club_colors",
        fmap CustomDemolish $ Json.required object "custom_demolish",
        fmap DamageState $ Json.required object "damage_state",
        fmap Demolish $ Json.required object "demolish",
        fmap Enum $ Json.required object "enum",
        fmap Explosion $ Json.required object "explosion",
        fmap ExtendedExplosion $ Json.required object "extended_explosion",
        fmap FlaggedByte $ Json.required object "flagged_byte",
        fmap FlaggedInt $ Json.required object "flagged_int",
        fmap Float $ Json.required object "float",
        fmap GameMode $ Json.required object "game_mode",
        fmap GameServer $ Json.required object "game_server",
        fmap Int $ Json.required object "int",
        fmap Int64 $ Json.required object "int64",
        fmap Loadout $ Json.required object "loadout",
        fmap LoadoutOnline $ Json.required object "loadout_online",
        fmap Loadouts $ Json.required object "loadouts",
        fmap LoadoutsOnline $ Json.required object "loadouts_online",
        fmap Location $ Json.required object "location",
        fmap MusicStinger $ Json.required object "music_stinger",
        fmap PartyLeader $ Json.required object "party_leader",
        fmap Pickup $ Json.required object "pickup",
        fmap PickupInfo $ Json.required object "pickup_info",
        fmap PickupNew $ Json.required object "pickup_new",
        fmap PlayerHistoryKey $ Json.required object "player_history_key",
        fmap PrivateMatchSettings $ Json.required object "private_match_settings",
        fmap QWord $ Json.required object "q_word",
        fmap RepStatTitle $ Json.required object "rep_stat_title",
        fmap Reservation $ Json.required object "reservation",
        fmap RigidBodyState $ Json.required object "rigid_body_state",
        fmap Rotation $ Json.required object "rotation",
        fmap StatEvent $ Json.required object "stat_event",
        fmap String $ Json.required object "string",
        fmap TeamPaint $ Json.required object "team_paint",
        fmap Title $ Json.required object "title",
        fmap UniqueId $ Json.required object "unique_id",
        fmap WeldedInfo $ Json.required object "welded_info"
      ]

instance Json.ToJSON AttributeValue where
  toJSON x = case x of
    AppliedDamage y -> Json.object [Json.pair "applied_damage" y]
    Boolean y -> Json.object [Json.pair "boolean" y]
    Byte y -> Json.object [Json.pair "byte" y]
    CamSettings y -> Json.object [Json.pair "cam_settings" y]
    ClubColors y -> Json.object [Json.pair "club_colors" y]
    CustomDemolish y -> Json.object [Json.pair "custom_demolish" y]
    DamageState y -> Json.object [Json.pair "damage_state" y]
    Demolish y -> Json.object [Json.pair "demolish" y]
    Enum y -> Json.object [Json.pair "enum" y]
    Explosion y -> Json.object [Json.pair "explosion" y]
    ExtendedExplosion y -> Json.object [Json.pair "extended_explosion" y]
    FlaggedByte y -> Json.object [Json.pair "flagged_byte" y]
    FlaggedInt y -> Json.object [Json.pair "flagged_int" y]
    Float y -> Json.object [Json.pair "float" y]
    GameMode y -> Json.object [Json.pair "game_mode" y]
    GameServer y -> Json.object [Json.pair "game_server" y]
    Int y -> Json.object [Json.pair "int" y]
    Int64 y -> Json.object [Json.pair "int64" y]
    Loadout y -> Json.object [Json.pair "loadout" y]
    LoadoutOnline y -> Json.object [Json.pair "loadout_online" y]
    Loadouts y -> Json.object [Json.pair "loadouts" y]
    LoadoutsOnline y -> Json.object [Json.pair "loadouts_online" y]
    Location y -> Json.object [Json.pair "location" y]
    MusicStinger y -> Json.object [Json.pair "music_stinger" y]
    PartyLeader y -> Json.object [Json.pair "party_leader" y]
    Pickup y -> Json.object [Json.pair "pickup" y]
    PickupInfo y -> Json.object [Json.pair "pickup_info" y]
    PickupNew y -> Json.object [Json.pair "pickup_new" y]
    PlayerHistoryKey y -> Json.object [Json.pair "player_history_key" y]
    PrivateMatchSettings y ->
      Json.object [Json.pair "private_match_settings" y]
    QWord y -> Json.object [Json.pair "q_word" y]
    RepStatTitle y -> Json.object [Json.pair "rep_stat_title" y]
    Reservation y -> Json.object [Json.pair "reservation" y]
    RigidBodyState y -> Json.object [Json.pair "rigid_body_state" y]
    Rotation y -> Json.object [Json.pair "rotation" y]
    StatEvent y -> Json.object [Json.pair "stat_event" y]
    String y -> Json.object [Json.pair "string" y]
    TeamPaint y -> Json.object [Json.pair "team_paint" y]
    Title y -> Json.object [Json.pair "title" y]
    UniqueId y -> Json.object [Json.pair "unique_id" y]
    WeldedInfo y -> Json.object [Json.pair "welded_info" y]

schema :: Schema.Schema
schema =
  Schema.named "attribute-value" . Schema.oneOf $
    fmap
      (\(k, v) -> Schema.object [(Json.pair k $ Schema.ref v, True)])
      [ ("applied_damage", AppliedDamage.schema),
        ("boolean", Boolean.schema),
        ("byte", Byte.schema),
        ("cam_settings", CamSettings.schema),
        ("club_colors", ClubColors.schema),
        ("custom_demolish", CustomDemolish.schema),
        ("damage_state", DamageState.schema),
        ("demolish", Demolish.schema),
        ("enum", Enum.schema),
        ("explosion", Explosion.schema),
        ("extended_explosion", ExtendedExplosion.schema),
        ("flagged_byte", FlaggedByte.schema),
        ("flagged_int", FlaggedInt.schema),
        ("float", Float.schema),
        ("game_mode", GameMode.schema),
        ("game_server", GameServer.schema),
        ("int", Int.schema),
        ("int64", Int64.schema),
        ("loadout_online", LoadoutOnline.schema),
        ("loadout", Loadout.schema),
        ("loadouts_online", LoadoutsOnline.schema),
        ("loadouts", Loadouts.schema),
        ("location", Location.schema),
        ("music_stinger", MusicStinger.schema),
        ("party_leader", PartyLeader.schema),
        ("pickup_info", PickupInfo.schema),
        ("pickup_new", PickupNew.schema),
        ("pickup", Pickup.schema),
        ("player_history_key", PlayerHistoryKey.schema),
        ("private_match_settings", PrivateMatchSettings.schema),
        ("q_word", QWord.schema),
        ("rep_stat_title", RepStatTitle.schema),
        ("reservation", Reservation.schema),
        ("rigid_body_state", RigidBodyState.schema),
        ("rotation", Rotation.schema),
        ("stat_event", StatEvent.schema),
        ("string", String.schema),
        ("team_paint", TeamPaint.schema),
        ("title", Title.schema),
        ("unique_id", UniqueId.schema),
        ("welded_info", WeldedInfo.schema)
      ]

bitPut :: AttributeValue -> BitPut.BitPut
bitPut value = case value of
  AppliedDamage x -> AppliedDamage.bitPut x
  Boolean x -> Boolean.bitPut x
  Byte x -> Byte.bitPut x
  CamSettings x -> CamSettings.bitPut x
  ClubColors x -> ClubColors.bitPut x
  CustomDemolish x -> CustomDemolish.bitPut x
  DamageState x -> DamageState.bitPut x
  Demolish x -> Demolish.bitPut x
  Enum x -> Enum.bitPut x
  Explosion x -> Explosion.bitPut x
  ExtendedExplosion x -> ExtendedExplosion.bitPut x
  FlaggedInt x -> FlaggedInt.bitPut x
  FlaggedByte x -> FlaggedByte.bitPut x
  Float x -> Float.bitPut x
  GameMode x -> GameMode.bitPut x
  GameServer x -> GameServer.bitPut x
  Int x -> Int.bitPut x
  Int64 x -> Int64.putInt64Attribute x
  Loadout x -> Loadout.bitPut x
  LoadoutOnline x -> LoadoutOnline.bitPut x
  Loadouts x -> Loadouts.bitPut x
  LoadoutsOnline x -> LoadoutsOnline.bitPut x
  Location x -> Location.bitPut x
  MusicStinger x -> MusicStinger.bitPut x
  PartyLeader x -> PartyLeader.bitPut x
  Pickup x -> Pickup.bitPut x
  PickupInfo x -> PickupInfo.bitPut x
  PickupNew x -> PickupNew.bitPut x
  PlayerHistoryKey x -> PlayerHistoryKey.bitPut x
  PrivateMatchSettings x -> PrivateMatchSettings.bitPut x
  QWord x -> QWord.bitPut x
  RepStatTitle x -> RepStatTitle.bitPut x
  Reservation x -> Reservation.bitPut x
  RigidBodyState x -> RigidBodyState.bitPut x
  Rotation x -> Rotation.bitPut x
  StatEvent x -> StatEvent.bitPut x
  String x -> String.bitPut x
  TeamPaint x -> TeamPaint.bitPut x
  Title x -> Title.bitPut x
  UniqueId x -> UniqueId.bitPut x
  WeldedInfo x -> WeldedInfo.bitPut x

bitGet ::
  Version.Version ->
  Maybe Str.Str ->
  Map.Map U32.U32 Str.Str ->
  Str.Str ->
  BitGet.BitGet AttributeValue
bitGet version buildVersion objectMap name =
  BitGet.label "AttributeValue" $ do
    constructor <- case Map.lookup (Str.toText name) Data.attributeTypes of
      Nothing ->
        BitGet.throw . UnknownAttribute.UnknownAttribute $ Str.toString name
      Just x -> pure x
    case constructor of
      AttributeType.AppliedDamage ->
        fmap AppliedDamage $ AppliedDamage.bitGet version
      AttributeType.Boolean -> fmap Boolean Boolean.bitGet
      AttributeType.Byte -> fmap Byte Byte.bitGet
      AttributeType.CamSettings ->
        fmap CamSettings $ CamSettings.bitGet version
      AttributeType.ClubColors -> fmap ClubColors ClubColors.bitGet
      AttributeType.CustomDemolish ->
        fmap CustomDemolish $ CustomDemolish.bitGet version
      AttributeType.DamageState ->
        fmap DamageState $ DamageState.bitGet version
      AttributeType.Demolish -> fmap Demolish $ Demolish.bitGet version
      AttributeType.Enum -> fmap Enum Enum.bitGet
      AttributeType.Explosion -> fmap Explosion $ Explosion.bitGet version
      AttributeType.ExtendedExplosion ->
        fmap ExtendedExplosion $ ExtendedExplosion.bitGet version
      AttributeType.FlaggedInt -> fmap FlaggedInt FlaggedInt.bitGet
      AttributeType.FlaggedByte -> fmap FlaggedByte FlaggedByte.bitGet
      AttributeType.Float -> fmap Float Float.bitGet
      AttributeType.GameMode -> fmap GameMode $ GameMode.bitGet version
      AttributeType.GameServer ->
        fmap GameServer $ GameServer.bitGet buildVersion
      AttributeType.Int -> fmap Int Int.bitGet
      AttributeType.Int64 -> fmap Int64 Int64.bitGet
      AttributeType.Loadout -> fmap Loadout Loadout.bitGet
      AttributeType.LoadoutOnline ->
        fmap LoadoutOnline $ LoadoutOnline.bitGet version objectMap
      AttributeType.Loadouts -> fmap Loadouts Loadouts.bitGet
      AttributeType.LoadoutsOnline ->
        fmap LoadoutsOnline $ LoadoutsOnline.bitGet version objectMap
      AttributeType.Location -> fmap Location $ Location.bitGet version
      AttributeType.MusicStinger -> fmap MusicStinger MusicStinger.bitGet
      AttributeType.PartyLeader ->
        fmap PartyLeader $ PartyLeader.bitGet version
      AttributeType.Pickup -> fmap Pickup Pickup.bitGet
      AttributeType.PickupInfo -> fmap PickupInfo PickupInfo.bitGet
      AttributeType.PickupNew -> fmap PickupNew PickupNew.bitGet
      AttributeType.PlayerHistoryKey ->
        fmap PlayerHistoryKey PlayerHistoryKey.bitGet
      AttributeType.PrivateMatchSettings ->
        fmap PrivateMatchSettings PrivateMatchSettings.bitGet
      AttributeType.QWord -> fmap QWord QWord.bitGet
      AttributeType.RepStatTitle -> fmap RepStatTitle RepStatTitle.bitGet
      AttributeType.Reservation ->
        fmap Reservation $ Reservation.bitGet version
      AttributeType.RigidBodyState ->
        fmap RigidBodyState $ RigidBodyState.bitGet version
      AttributeType.Rotation -> fmap Rotation Rotation.bitGet
      AttributeType.StatEvent -> fmap StatEvent StatEvent.bitGet
      AttributeType.String -> fmap String String.bitGet
      AttributeType.TeamPaint -> fmap TeamPaint TeamPaint.bitGet
      AttributeType.Title -> fmap Title Title.bitGet
      AttributeType.UniqueId -> fmap UniqueId $ UniqueId.bitGet version
      AttributeType.WeldedInfo -> fmap WeldedInfo $ WeldedInfo.bitGet version
