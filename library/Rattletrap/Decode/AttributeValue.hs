module Rattletrap.Decode.AttributeValue
  ( decodeAttributeValueBits
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Data
import Rattletrap.Decode.AppliedDamageAttribute
import Rattletrap.Decode.BooleanAttribute
import Rattletrap.Decode.ByteAttribute
import Rattletrap.Decode.CamSettingsAttribute
import Rattletrap.Decode.ClubColorsAttribute
import Rattletrap.Decode.Common
import Rattletrap.Decode.DamageStateAttribute
import Rattletrap.Decode.DemolishAttribute
import Rattletrap.Decode.EnumAttribute
import Rattletrap.Decode.ExplosionAttribute
import Rattletrap.Decode.ExtendedExplosionAttribute
import Rattletrap.Decode.FlaggedIntAttribute
import Rattletrap.Decode.FloatAttribute
import Rattletrap.Decode.GameModeAttribute
import Rattletrap.Decode.IntAttribute
import Rattletrap.Decode.Int64Attribute
import Rattletrap.Decode.LoadoutAttribute
import Rattletrap.Decode.LoadoutOnlineAttribute
import Rattletrap.Decode.LoadoutsAttribute
import Rattletrap.Decode.LoadoutsOnlineAttribute
import Rattletrap.Decode.LocationAttribute
import Rattletrap.Decode.MusicStingerAttribute
import Rattletrap.Decode.PartyLeaderAttribute
import Rattletrap.Decode.PickupAttribute
import Rattletrap.Decode.PlayerHistoryKeyAttribute
import Rattletrap.Decode.PrivateMatchSettingsAttribute
import Rattletrap.Decode.QWordAttribute
import Rattletrap.Decode.ReservationAttribute
import Rattletrap.Decode.RigidBodyStateAttribute
import Rattletrap.Decode.StatEventAttribute
import Rattletrap.Decode.StringAttribute
import Rattletrap.Decode.TeamPaintAttribute
import Rattletrap.Decode.TitleAttribute
import Rattletrap.Decode.UniqueIdAttribute
import Rattletrap.Decode.WeldedInfoAttribute
import Rattletrap.Type.AttributeType
import Rattletrap.Type.AttributeValue
import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Data.Map as Map

decodeAttributeValueBits
  :: (Int, Int, Int) -> Map Word32le Str -> Str -> DecodeBits AttributeValue
decodeAttributeValueBits version objectMap name = do
  constructor <- maybe
    (fail ("don't know how to get attribute value " <> show name))
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
    AttributeTypeDamageState ->
      AttributeValueDamageState <$> decodeDamageStateAttributeBits version
    AttributeTypeDemolish ->
      AttributeValueDemolish <$> decodeDemolishAttributeBits version
    AttributeTypeEnum -> AttributeValueEnum <$> decodeEnumAttributeBits
    AttributeTypeExplosion ->
      AttributeValueExplosion <$> decodeExplosionAttributeBits version
    AttributeTypeExtendedExplosion ->
      AttributeValueExtendedExplosion <$> decodeExtendedExplosionAttributeBits version
    AttributeTypeFlaggedInt ->
      AttributeValueFlaggedInt <$> decodeFlaggedIntAttributeBits
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
    AttributeTypePlayerHistoryKey ->
      AttributeValuePlayerHistoryKey <$> decodePlayerHistoryKeyAttributeBits
    AttributeTypePrivateMatchSettings ->
      AttributeValuePrivateMatchSettings
        <$> decodePrivateMatchSettingsAttributeBits
    AttributeTypeQWord -> AttributeValueQWord <$> decodeQWordAttributeBits
    AttributeTypeReservation ->
      AttributeValueReservation <$> decodeReservationAttributeBits version
    AttributeTypeRigidBodyState ->
      AttributeValueRigidBodyState <$> decodeRigidBodyStateAttributeBits version
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
