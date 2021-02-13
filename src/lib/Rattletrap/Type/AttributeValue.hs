{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeValue where

import Rattletrap.Type.Common
import Rattletrap.Type.Attribute.AppliedDamage
import Rattletrap.Type.Attribute.Boolean
import Rattletrap.Type.Attribute.Byte
import Rattletrap.Type.Attribute.CamSettings
import Rattletrap.Type.Attribute.ClubColors
import Rattletrap.Type.Attribute.CustomDemolish
import Rattletrap.Type.Attribute.DamageState
import Rattletrap.Type.Attribute.Demolish
import Rattletrap.Type.Attribute.Enum
import Rattletrap.Type.Attribute.Explosion
import Rattletrap.Type.Attribute.ExtendedExplosion
import Rattletrap.Type.Attribute.FlaggedByte
import Rattletrap.Type.Attribute.FlaggedInt
import Rattletrap.Type.Attribute.Float
import Rattletrap.Type.Attribute.GameMode
import Rattletrap.Type.Attribute.Int64
import Rattletrap.Type.Attribute.Int
import Rattletrap.Type.Attribute.Loadout
import Rattletrap.Type.Attribute.LoadoutOnline
import Rattletrap.Type.Attribute.Loadouts
import Rattletrap.Type.Attribute.LoadoutsOnline
import Rattletrap.Type.Attribute.Location
import Rattletrap.Type.Attribute.MusicStinger
import Rattletrap.Type.Attribute.PartyLeader
import Rattletrap.Type.Attribute.Pickup
import Rattletrap.Type.Attribute.PickupNew
import Rattletrap.Type.Attribute.PlayerHistoryKey
import Rattletrap.Type.Attribute.PrivateMatchSettings
import Rattletrap.Type.Attribute.QWord
import Rattletrap.Type.Attribute.Reservation
import Rattletrap.Type.Attribute.RigidBodyState
import Rattletrap.Type.Attribute.StatEvent
import Rattletrap.Type.Attribute.String
import Rattletrap.Type.Attribute.TeamPaint
import Rattletrap.Type.Attribute.Title
import Rattletrap.Type.Attribute.UniqueId
import Rattletrap.Type.Attribute.WeldedInfo
import qualified Rattletrap.Data as Data
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.AttributeType as AttributeType
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common

import qualified Data.Map as Map

data AttributeValue
  = AppliedDamage AppliedDamageAttribute
  | Boolean BooleanAttribute
  | Byte ByteAttribute
  | CamSettings CamSettingsAttribute
  | ClubColors ClubColorsAttribute
  | CustomDemolish CustomDemolishAttribute
  | DamageState DamageStateAttribute
  | Demolish DemolishAttribute
  | Enum EnumAttribute
  | Explosion ExplosionAttribute
  | ExtendedExplosion ExtendedExplosionAttribute
  | FlaggedInt FlaggedIntAttribute
  | FlaggedByte FlaggedByteAttribute
  | Float FloatAttribute
  | GameMode GameModeAttribute
  | Int IntAttribute
  | Int64 Int64Attribute
  | Loadout LoadoutAttribute
  | LoadoutOnline LoadoutOnlineAttribute
  | Loadouts LoadoutsAttribute
  | LoadoutsOnline LoadoutsOnlineAttribute
  | Location LocationAttribute
  | MusicStinger MusicStingerAttribute
  | PartyLeader PartyLeaderAttribute
  | Pickup PickupAttribute
  | PickupNew PickupAttributeNew
  | PlayerHistoryKey PlayerHistoryKeyAttribute
  | PrivateMatchSettings PrivateMatchSettingsAttribute
  | QWord QWordAttribute
  | Reservation ReservationAttribute
  | RigidBodyState RigidBodyStateAttribute
  | StatEvent StatEventAttribute
  | String StringAttribute
  | TeamPaint TeamPaintAttribute
  | Title TitleAttribute
  | UniqueId UniqueIdAttribute
  | WeldedInfo WeldedInfoAttribute
  deriving (Eq, Show)

$(deriveJsonWith ''AttributeValue jsonOptions)

bitPut :: AttributeValue -> BitPut ()
bitPut value = case value of
  AppliedDamage x -> putAppliedDamageAttribute x
  Boolean x -> putBooleanAttribute x
  Byte x -> putByteAttribute x
  CamSettings x -> putCamSettingsAttribute x
  ClubColors x -> putClubColorsAttribute x
  CustomDemolish x -> putCustomDemolishAttribute x
  DamageState x -> putDamageStateAttribute x
  Demolish x -> putDemolishAttribute x
  Enum x -> putEnumAttribute x
  Explosion x -> putExplosionAttribute x
  ExtendedExplosion x -> putExtendedExplosionAttribute x
  FlaggedInt x -> putFlaggedIntAttribute x
  FlaggedByte x -> putFlaggedByteAttribute x
  Float x -> putFloatAttribute x
  GameMode x -> putGameModeAttribute x
  Int x -> putIntAttribute x
  Int64 x -> putInt64Attribute x
  Loadout x -> putLoadoutAttribute x
  LoadoutOnline x -> putLoadoutOnlineAttribute x
  Loadouts x -> putLoadoutsAttribute x
  LoadoutsOnline x -> putLoadoutsOnlineAttribute x
  Location x -> putLocationAttribute x
  MusicStinger x -> putMusicStingerAttribute x
  PartyLeader x -> putPartyLeaderAttribute x
  Pickup x -> putPickupAttribute x
  PickupNew x -> putPickupAttributeNew x
  PlayerHistoryKey x -> putPlayerHistoryKeyAttribute x
  PrivateMatchSettings x -> putPrivateMatchSettingsAttribute x
  QWord x -> putQWordAttribute x
  Reservation x -> putReservationAttribute x
  RigidBodyState x -> putRigidBodyStateAttribute x
  StatEvent x -> putStatEventAttribute x
  String x -> putStringAttribute x
  TeamPaint x -> putTeamPaintAttribute x
  Title x -> putTitleAttribute x
  UniqueId x -> putUniqueIdAttribute x
  WeldedInfo x -> putWeldedInfoAttribute x

bitGet
  :: (Int, Int, Int) -> Map Word32le.Word32le Str.Str -> Str.Str -> BitGet AttributeValue
bitGet version objectMap name = do
  constructor <- maybe
    (fail ("[RT04] don't know how to get attribute value " <> show name))
    pure
    (Map.lookup (Str.toText name) Data.attributeTypes)
  case constructor of
    AttributeType.AppliedDamage -> AppliedDamage <$> decodeAppliedDamageAttributeBits version
    AttributeType.Boolean -> Boolean <$> decodeBooleanAttributeBits
    AttributeType.Byte -> Byte <$> decodeByteAttributeBits
    AttributeType.CamSettings -> CamSettings <$> decodeCamSettingsAttributeBits version
    AttributeType.ClubColors -> ClubColors <$> decodeClubColorsAttributeBits
    AttributeType.CustomDemolish -> CustomDemolish <$> decodeCustomDemolishAttributeBits version
    AttributeType.DamageState -> DamageState <$> decodeDamageStateAttributeBits version
    AttributeType.Demolish -> Demolish <$> decodeDemolishAttributeBits version
    AttributeType.Enum -> Enum <$> decodeEnumAttributeBits
    AttributeType.Explosion -> Explosion <$> decodeExplosionAttributeBits version
    AttributeType.ExtendedExplosion -> ExtendedExplosion <$> decodeExtendedExplosionAttributeBits version
    AttributeType.FlaggedInt -> FlaggedInt <$> decodeFlaggedIntAttributeBits
    AttributeType.FlaggedByte -> FlaggedByte <$> decodeFlaggedByteAttributeBits
    AttributeType.Float -> Float <$> decodeFloatAttributeBits
    AttributeType.GameMode -> GameMode <$> decodeGameModeAttributeBits version
    AttributeType.Int -> Int <$> decodeIntAttributeBits
    AttributeType.Int64 -> Int64 <$> decodeInt64AttributeBits
    AttributeType.Loadout -> Loadout <$> decodeLoadoutAttributeBits
    AttributeType.LoadoutOnline -> LoadoutOnline <$> decodeLoadoutOnlineAttributeBits version objectMap
    AttributeType.Loadouts -> Loadouts <$> decodeLoadoutsAttributeBits
    AttributeType.LoadoutsOnline -> LoadoutsOnline <$> decodeLoadoutsOnlineAttributeBits version objectMap
    AttributeType.Location -> Location <$> decodeLocationAttributeBits version
    AttributeType.MusicStinger -> MusicStinger <$> decodeMusicStingerAttributeBits
    AttributeType.PartyLeader -> PartyLeader <$> decodePartyLeaderAttributeBits version
    AttributeType.Pickup -> Pickup <$> decodePickupAttributeBits
    AttributeType.PickupNew -> PickupNew <$> decodePickupAttributeNewBits
    AttributeType.PlayerHistoryKey -> PlayerHistoryKey <$> decodePlayerHistoryKeyAttributeBits
    AttributeType.PrivateMatchSettings -> PrivateMatchSettings <$> decodePrivateMatchSettingsAttributeBits
    AttributeType.QWord -> QWord <$> decodeQWordAttributeBits
    AttributeType.Reservation -> Reservation <$> decodeReservationAttributeBits version
    AttributeType.RigidBodyState -> RigidBodyState <$> decodeRigidBodyStateAttributeBits version
    AttributeType.StatEvent -> StatEvent <$> decodeStatEventAttributeBits
    AttributeType.String -> String <$> decodeStringAttributeBits
    AttributeType.TeamPaint -> TeamPaint <$> decodeTeamPaintAttributeBits
    AttributeType.Title -> Title <$> decodeTitleAttributeBits
    AttributeType.UniqueId -> UniqueId <$> decodeUniqueIdAttributeBits version
    AttributeType.WeldedInfo -> WeldedInfo <$> decodeWeldedInfoAttributeBits version
