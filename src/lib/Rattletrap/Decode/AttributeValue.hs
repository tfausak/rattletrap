module Rattletrap.Decode.AttributeValue
  ( decodeAttributeValueBits
  )
where

import Rattletrap.Data
import Rattletrap.Decode.AppliedDamageAttribute
import Rattletrap.Decode.BooleanAttribute
import Rattletrap.Decode.ByteAttribute
import Rattletrap.Decode.CamSettingsAttribute
import Rattletrap.Decode.ClubColorsAttribute
import Rattletrap.Decode.Common
import Rattletrap.Decode.CustomDemolishAttribute
import Rattletrap.Decode.DamageStateAttribute
import Rattletrap.Decode.DemolishAttribute
import Rattletrap.Decode.EnumAttribute
import Rattletrap.Decode.ExplosionAttribute
import Rattletrap.Decode.ExtendedExplosionAttribute
import Rattletrap.Decode.FlaggedIntAttribute
import Rattletrap.Decode.FlaggedByteAttribute
import Rattletrap.Decode.FloatAttribute
import Rattletrap.Decode.GameModeAttribute
import Rattletrap.Decode.Int64Attribute
import Rattletrap.Decode.IntAttribute
import Rattletrap.Decode.LoadoutAttribute
import Rattletrap.Decode.LoadoutOnlineAttribute
import Rattletrap.Decode.LoadoutsAttribute
import Rattletrap.Decode.LoadoutsOnlineAttribute
import Rattletrap.Decode.LocationAttribute
import Rattletrap.Decode.MusicStingerAttribute
import Rattletrap.Decode.PartyLeaderAttribute
import Rattletrap.Decode.PickupAttribute
import Rattletrap.Decode.PickupAttributeNew
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
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str

import qualified Data.Map as Map

decodeAttributeValueBits
  :: (Int, Int, Int) -> ClassAttributeMap -> CompressedWord -> Str -> DecodeBits AttributeValue
decodeAttributeValueBits version classes attribute name = do
  constructor <- getAttributeType classes attribute name
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
    AttributeTypeCustomDemolish ->
      AttributeValueCustomDemolish <$> decodeCustomDemolishAttributeBits version
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
        <$> decodeLoadoutOnlineAttributeBits version (classAttributeMapObjectMap classes)
    AttributeTypeLoadouts ->
      AttributeValueLoadouts <$> decodeLoadoutsAttributeBits
    AttributeTypeLoadoutsOnline ->
      AttributeValueLoadoutsOnline
        <$> decodeLoadoutsOnlineAttributeBits version (classAttributeMapObjectMap classes)
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

getAttributeType :: MonadFail m => ClassAttributeMap -> CompressedWord -> Str -> m AttributeType
getAttributeType _ _ name = maybe
  (fail ("[RT04] don't know how to get attribute value " <> show name))
  pure
  (Map.lookup name attributeTypes)

attributeTypes :: Map Str AttributeType
attributeTypes = Map.mapKeys toStr (Map.fromList rawAttributeTypes)
