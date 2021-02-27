module Rattletrap.Type.AttributeValue where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Data as Data
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
import qualified Rattletrap.Type.Attribute.PickupNew as PickupNew
import qualified Rattletrap.Type.Attribute.PlayerHistoryKey as PlayerHistoryKey
import qualified Rattletrap.Type.Attribute.PrivateMatchSettings as PrivateMatchSettings
import qualified Rattletrap.Type.Attribute.QWord as QWord
import qualified Rattletrap.Type.Attribute.Reservation as Reservation
import qualified Rattletrap.Type.Attribute.RigidBodyState as RigidBodyState
import qualified Rattletrap.Type.Attribute.StatEvent as StatEvent
import qualified Rattletrap.Type.Attribute.String as String
import qualified Rattletrap.Type.Attribute.TeamPaint as TeamPaint
import qualified Rattletrap.Type.Attribute.Title as Title
import qualified Rattletrap.Type.Attribute.UniqueId as UniqueId
import qualified Rattletrap.Type.Attribute.WeldedInfo as WeldedInfo
import qualified Rattletrap.Type.AttributeType as AttributeType
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

import qualified Data.Map as Map

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
  | PickupNew PickupNew.PickupNew
  | PlayerHistoryKey PlayerHistoryKey.PlayerHistoryKey
  | PrivateMatchSettings PrivateMatchSettings.PrivateMatchSettings
  | QWord QWord.QWord
  | Reservation Reservation.Reservation
  | RigidBodyState RigidBodyState.RigidBodyState
  | StatEvent StatEvent.StatEvent
  | String String.String
  | TeamPaint TeamPaint.TeamPaint
  | Title Title.Title
  | UniqueId UniqueId.UniqueId
  | WeldedInfo WeldedInfo.WeldedInfo
  deriving (Eq, Show)

$(deriveJson ''AttributeValue)

schema :: Schema.Schema
schema = Schema.named "attribute-value" . Schema.oneOf $ fmap
  (\(k, v) -> Schema.object [(Json.pair k $ Schema.ref v, True)])
  [ ("applied_damage", AppliedDamage.schema)
  , ("boolean", Boolean.schema)
  , ("byte", Byte.schema)
  , ("cam_settings", CamSettings.schema)
  , ("club_colors", ClubColors.schema)
  , ("custom_demolish", CustomDemolish.schema)
  , ("damage_state", DamageState.schema)
  , ("demolish", Demolish.schema)
  , ("enum", Enum.schema)
  , ("explosion", Explosion.schema)
  , ("extended_explosion", ExtendedExplosion.schema)
  , ("flagged_byte", FlaggedByte.schema)
  , ("flagged_int", FlaggedInt.schema)
  , ("float", Float.schema)
  , ("game_mode", GameMode.schema)
  , ("int", Int.schema)
  , ("int64", Int64.schema)
  , ("loadout_online", LoadoutOnline.schema)
  , ("loadout", Loadout.schema)
  , ("loadouts_online", LoadoutsOnline.schema)
  , ("loadouts", Loadouts.schema)
  , ("location", Location.schema)
  , ("music_stinger", MusicStinger.schema)
  , ("party_leader", PartyLeader.schema)
  , ("pickup_new", PickupNew.schema)
  , ("pickup", Pickup.schema)
  , ("player_history_key", PlayerHistoryKey.schema)
  , ("private_match_settings", PrivateMatchSettings.schema)
  , ("q_word", QWord.schema)
  , ("reservation", Reservation.schema)
  , ("rigid_body_state", RigidBodyState.schema)
  , ("stat_event", StatEvent.schema)
  , ("string", String.schema)
  , ("team_paint", TeamPaint.schema)
  , ("title", Title.schema)
  , ("unique_id", UniqueId.schema)
  , ("welded_info", WeldedInfo.schema)
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
  PickupNew x -> PickupNew.bitPut x
  PlayerHistoryKey x -> PlayerHistoryKey.bitPut x
  PrivateMatchSettings x -> PrivateMatchSettings.bitPut x
  QWord x -> QWord.bitPut x
  Reservation x -> Reservation.bitPut x
  RigidBodyState x -> RigidBodyState.bitPut x
  StatEvent x -> StatEvent.bitPut x
  String x -> String.bitPut x
  TeamPaint x -> TeamPaint.bitPut x
  Title x -> Title.bitPut x
  UniqueId x -> UniqueId.bitPut x
  WeldedInfo x -> WeldedInfo.bitPut x

bitGet
  :: Version.Version
  -> Map U32.U32 Str.Str
  -> Str.Str
  -> BitGet.BitGet AttributeValue
bitGet version objectMap name = do
  constructor <- maybe
    (fail ("[RT04] don't know how to get attribute value " <> show name))
    pure
    (Map.lookup (Str.toText name) Data.attributeTypes)
  case constructor of
    AttributeType.AppliedDamage ->
      AppliedDamage <$> AppliedDamage.bitGet version
    AttributeType.Boolean -> Boolean <$> Boolean.bitGet
    AttributeType.Byte -> Byte <$> Byte.bitGet
    AttributeType.CamSettings -> CamSettings <$> CamSettings.bitGet version
    AttributeType.ClubColors -> ClubColors <$> ClubColors.bitGet
    AttributeType.CustomDemolish ->
      CustomDemolish <$> CustomDemolish.bitGet version
    AttributeType.DamageState -> DamageState <$> DamageState.bitGet version
    AttributeType.Demolish -> Demolish <$> Demolish.bitGet version
    AttributeType.Enum -> Enum <$> Enum.bitGet
    AttributeType.Explosion -> Explosion <$> Explosion.bitGet version
    AttributeType.ExtendedExplosion ->
      ExtendedExplosion <$> ExtendedExplosion.bitGet version
    AttributeType.FlaggedInt -> FlaggedInt <$> FlaggedInt.bitGet
    AttributeType.FlaggedByte -> FlaggedByte <$> FlaggedByte.bitGet
    AttributeType.Float -> Float <$> Float.bitGet
    AttributeType.GameMode -> GameMode <$> GameMode.bitGet version
    AttributeType.Int -> Int <$> Int.bitGet
    AttributeType.Int64 -> Int64 <$> Int64.bitGet
    AttributeType.Loadout -> Loadout <$> Loadout.bitGet
    AttributeType.LoadoutOnline ->
      LoadoutOnline <$> LoadoutOnline.bitGet version objectMap
    AttributeType.Loadouts -> Loadouts <$> Loadouts.bitGet
    AttributeType.LoadoutsOnline ->
      LoadoutsOnline <$> LoadoutsOnline.bitGet version objectMap
    AttributeType.Location -> Location <$> Location.bitGet version
    AttributeType.MusicStinger -> MusicStinger <$> MusicStinger.bitGet
    AttributeType.PartyLeader -> PartyLeader <$> PartyLeader.bitGet version
    AttributeType.Pickup -> Pickup <$> Pickup.bitGet
    AttributeType.PickupNew -> PickupNew <$> PickupNew.bitGet
    AttributeType.PlayerHistoryKey ->
      PlayerHistoryKey <$> PlayerHistoryKey.bitGet
    AttributeType.PrivateMatchSettings ->
      PrivateMatchSettings <$> PrivateMatchSettings.bitGet
    AttributeType.QWord -> QWord <$> QWord.bitGet
    AttributeType.Reservation -> Reservation <$> Reservation.bitGet version
    AttributeType.RigidBodyState ->
      RigidBodyState <$> RigidBodyState.bitGet version
    AttributeType.StatEvent -> StatEvent <$> StatEvent.bitGet
    AttributeType.String -> String <$> String.bitGet
    AttributeType.TeamPaint -> TeamPaint <$> TeamPaint.bitGet
    AttributeType.Title -> Title <$> Title.bitGet
    AttributeType.UniqueId -> UniqueId <$> UniqueId.bitGet version
    AttributeType.WeldedInfo -> WeldedInfo <$> WeldedInfo.bitGet version
