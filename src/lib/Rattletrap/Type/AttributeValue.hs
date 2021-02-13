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
import Rattletrap.Data
import Rattletrap.Decode.Common
import Rattletrap.Type.AttributeType
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data AttributeValue
  = AttributeValueAppliedDamage AppliedDamageAttribute
  | AttributeValueBoolean BooleanAttribute
  | AttributeValueByte ByteAttribute
  | AttributeValueCamSettings CamSettingsAttribute
  | AttributeValueClubColors ClubColorsAttribute
  | AttributeValueCustomDemolish CustomDemolishAttribute
  | AttributeValueDamageState DamageStateAttribute
  | AttributeValueDemolish DemolishAttribute
  | AttributeValueEnum EnumAttribute
  | AttributeValueExplosion ExplosionAttribute
  | AttributeValueExtendedExplosion ExtendedExplosionAttribute
  | AttributeValueFlaggedInt FlaggedIntAttribute
  | AttributeValueFlaggedByte FlaggedByteAttribute
  | AttributeValueFloat FloatAttribute
  | AttributeValueGameMode GameModeAttribute
  | AttributeValueInt IntAttribute
  | AttributeValueInt64 Int64Attribute
  | AttributeValueLoadout LoadoutAttribute
  | AttributeValueLoadoutOnline LoadoutOnlineAttribute
  | AttributeValueLoadouts LoadoutsAttribute
  | AttributeValueLoadoutsOnline LoadoutsOnlineAttribute
  | AttributeValueLocation LocationAttribute
  | AttributeValueMusicStinger MusicStingerAttribute
  | AttributeValuePartyLeader PartyLeaderAttribute
  | AttributeValuePickup PickupAttribute
  | AttributeValuePickupNew PickupAttributeNew
  | AttributeValuePlayerHistoryKey PlayerHistoryKeyAttribute
  | AttributeValuePrivateMatchSettings PrivateMatchSettingsAttribute
  | AttributeValueQWord QWordAttribute
  | AttributeValueReservation ReservationAttribute
  | AttributeValueRigidBodyState RigidBodyStateAttribute
  | AttributeValueStatEvent StatEventAttribute
  | AttributeValueString StringAttribute
  | AttributeValueTeamPaint TeamPaintAttribute
  | AttributeValueTitle TitleAttribute
  | AttributeValueUniqueId UniqueIdAttribute
  | AttributeValueWeldedInfo WeldedInfoAttribute
  deriving (Eq, Show)

$(deriveJson ''AttributeValue)

putAttributeValue :: AttributeValue -> BinaryBits.BitPut ()
putAttributeValue value = case value of
  AttributeValueAppliedDamage x -> putAppliedDamageAttribute x
  AttributeValueBoolean x -> putBooleanAttribute x
  AttributeValueByte x -> putByteAttribute x
  AttributeValueCamSettings x -> putCamSettingsAttribute x
  AttributeValueClubColors x -> putClubColorsAttribute x
  AttributeValueCustomDemolish x -> putCustomDemolishAttribute x
  AttributeValueDamageState x -> putDamageStateAttribute x
  AttributeValueDemolish x -> putDemolishAttribute x
  AttributeValueEnum x -> putEnumAttribute x
  AttributeValueExplosion x -> putExplosionAttribute x
  AttributeValueExtendedExplosion x -> putExtendedExplosionAttribute x
  AttributeValueFlaggedInt x -> putFlaggedIntAttribute x
  AttributeValueFlaggedByte x -> putFlaggedByteAttribute x
  AttributeValueFloat x -> putFloatAttribute x
  AttributeValueGameMode x -> putGameModeAttribute x
  AttributeValueInt x -> putIntAttribute x
  AttributeValueInt64 x -> putInt64Attribute x
  AttributeValueLoadout x -> putLoadoutAttribute x
  AttributeValueLoadoutOnline x -> putLoadoutOnlineAttribute x
  AttributeValueLoadouts x -> putLoadoutsAttribute x
  AttributeValueLoadoutsOnline x -> putLoadoutsOnlineAttribute x
  AttributeValueLocation x -> putLocationAttribute x
  AttributeValueMusicStinger x -> putMusicStingerAttribute x
  AttributeValuePartyLeader x -> putPartyLeaderAttribute x
  AttributeValuePickup x -> putPickupAttribute x
  AttributeValuePickupNew x -> putPickupAttributeNew x
  AttributeValuePlayerHistoryKey x -> putPlayerHistoryKeyAttribute x
  AttributeValuePrivateMatchSettings x -> putPrivateMatchSettingsAttribute x
  AttributeValueQWord x -> putQWordAttribute x
  AttributeValueReservation x -> putReservationAttribute x
  AttributeValueRigidBodyState x -> putRigidBodyStateAttribute x
  AttributeValueStatEvent x -> putStatEventAttribute x
  AttributeValueString x -> putStringAttribute x
  AttributeValueTeamPaint x -> putTeamPaintAttribute x
  AttributeValueTitle x -> putTitleAttribute x
  AttributeValueUniqueId x -> putUniqueIdAttribute x
  AttributeValueWeldedInfo x -> putWeldedInfoAttribute x

decodeAttributeValueBits
  :: (Int, Int, Int) -> Map Word32le Str -> Str -> DecodeBits AttributeValue
decodeAttributeValueBits version objectMap name = do
  constructor <- maybe
    (fail ("[RT04] don't know how to get attribute value " <> show name))
    pure
    (Map.lookup name attributeTypes)
  case constructor of
    AttributeTypeAppliedDamage ->
      AttributeValueAppliedDamage <$> decodeAppliedDamageAttributeBits version
    AttributeTypeBoolean ->
      AttributeValueBoolean <$> decodeBooleanAttributeBits
    AttributeTypeByte -> AttributeValueByte <$> decodeByteAttributeBits
    AttributeTypeCamSettings ->
      AttributeValueCamSettings <$> decodeCamSettingsAttributeBits version
    AttributeTypeClubColors ->
      AttributeValueClubColors <$> decodeClubColorsAttributeBits
    AttributeTypeCustomDemolish -> AttributeValueCustomDemolish
      <$> decodeCustomDemolishAttributeBits version
    AttributeTypeDamageState ->
      AttributeValueDamageState <$> decodeDamageStateAttributeBits version
    AttributeTypeDemolish ->
      AttributeValueDemolish <$> decodeDemolishAttributeBits version
    AttributeTypeEnum -> AttributeValueEnum <$> decodeEnumAttributeBits
    AttributeTypeExplosion ->
      AttributeValueExplosion <$> decodeExplosionAttributeBits version
    AttributeTypeExtendedExplosion -> AttributeValueExtendedExplosion
      <$> decodeExtendedExplosionAttributeBits version
    AttributeTypeFlaggedInt ->
      AttributeValueFlaggedInt <$> decodeFlaggedIntAttributeBits
    AttributeTypeFlaggedByte ->
      AttributeValueFlaggedByte <$> decodeFlaggedByteAttributeBits
    AttributeTypeFloat -> AttributeValueFloat <$> decodeFloatAttributeBits
    AttributeTypeGameMode ->
      AttributeValueGameMode <$> decodeGameModeAttributeBits version
    AttributeTypeInt -> AttributeValueInt <$> decodeIntAttributeBits
    AttributeTypeInt64 -> AttributeValueInt64 <$> decodeInt64AttributeBits
    AttributeTypeLoadout ->
      AttributeValueLoadout <$> decodeLoadoutAttributeBits
    AttributeTypeLoadoutOnline ->
      AttributeValueLoadoutOnline
        <$> decodeLoadoutOnlineAttributeBits version objectMap
    AttributeTypeLoadouts ->
      AttributeValueLoadouts <$> decodeLoadoutsAttributeBits
    AttributeTypeLoadoutsOnline ->
      AttributeValueLoadoutsOnline
        <$> decodeLoadoutsOnlineAttributeBits version objectMap
    AttributeTypeLocation ->
      AttributeValueLocation <$> decodeLocationAttributeBits version
    AttributeTypeMusicStinger ->
      AttributeValueMusicStinger <$> decodeMusicStingerAttributeBits
    AttributeTypePartyLeader ->
      AttributeValuePartyLeader <$> decodePartyLeaderAttributeBits version
    AttributeTypePickup -> AttributeValuePickup <$> decodePickupAttributeBits
    AttributeTypePickupNew ->
      AttributeValuePickupNew <$> decodePickupAttributeNewBits
    AttributeTypePlayerHistoryKey ->
      AttributeValuePlayerHistoryKey <$> decodePlayerHistoryKeyAttributeBits
    AttributeTypePrivateMatchSettings ->
      AttributeValuePrivateMatchSettings
        <$> decodePrivateMatchSettingsAttributeBits
    AttributeTypeQWord -> AttributeValueQWord <$> decodeQWordAttributeBits
    AttributeTypeReservation ->
      AttributeValueReservation <$> decodeReservationAttributeBits version
    AttributeTypeRigidBodyState -> AttributeValueRigidBodyState
      <$> decodeRigidBodyStateAttributeBits version
    AttributeTypeStatEvent ->
      AttributeValueStatEvent <$> decodeStatEventAttributeBits
    AttributeTypeString -> AttributeValueString <$> decodeStringAttributeBits
    AttributeTypeTeamPaint ->
      AttributeValueTeamPaint <$> decodeTeamPaintAttributeBits
    AttributeTypeTitle -> AttributeValueTitle <$> decodeTitleAttributeBits
    AttributeTypeUniqueId ->
      AttributeValueUniqueId <$> decodeUniqueIdAttributeBits version
    AttributeTypeWeldedInfo ->
      AttributeValueWeldedInfo <$> decodeWeldedInfoAttributeBits version

attributeTypes :: Map Str AttributeType
attributeTypes = Map.mapKeys toStr (Map.fromList rawAttributeTypes)
