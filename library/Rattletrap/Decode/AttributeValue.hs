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
import Rattletrap.Decode.LoadoutAttribute
import Rattletrap.Decode.LoadoutOnlineAttribute
import Rattletrap.Decode.LoadoutsAttribute
import Rattletrap.Decode.LoadoutsOnlineAttribute
import Rattletrap.Decode.LocationAttribute
import Rattletrap.Decode.MusicStingerAttribute
import Rattletrap.Decode.PartyLeaderAttribute
import Rattletrap.Decode.PickupAttribute
import Rattletrap.Decode.PrivateMatchSettingsAttribute
import Rattletrap.Decode.QWordAttribute
import Rattletrap.Decode.ReservationAttribute
import Rattletrap.Decode.RigidBodyStateAttribute
import Rattletrap.Decode.StringAttribute
import Rattletrap.Decode.TeamPaintAttribute
import Rattletrap.Decode.UniqueIdAttribute
import Rattletrap.Decode.WeldedInfoAttribute
import Rattletrap.Type.AttributeType
import Rattletrap.Type.AttributeValue
import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map

decodeAttributeValueBits
  :: Str
  -> Reader.ReaderT
       ((Int, Int, Int), Map Word32le Str)
       DecodeBits
       AttributeValue
decodeAttributeValueBits name = do
  constructor <- maybe
    (fail ("don't know how to get attribute value " <> show name))
    pure
    (Map.lookup name attributeTypes)
  case constructor of
    AttributeTypeAppliedDamage -> AttributeValueAppliedDamage <$> Trans.lift decodeAppliedDamageAttributeBits
    AttributeTypeBoolean -> AttributeValueBoolean <$> Trans.lift decodeBooleanAttributeBits
    AttributeTypeByte -> AttributeValueByte <$> Trans.lift decodeByteAttributeBits
    AttributeTypeCamSettings -> AttributeValueCamSettings <$> Reader.withReaderT fst decodeCamSettingsAttributeBits
    AttributeTypeClubColors -> AttributeValueClubColors <$> Trans.lift decodeClubColorsAttributeBits
    AttributeTypeDamageState -> AttributeValueDamageState <$> Trans.lift decodeDamageStateAttributeBits
    AttributeTypeDemolish -> AttributeValueDemolish <$> Trans.lift decodeDemolishAttributeBits
    AttributeTypeEnum -> AttributeValueEnum <$> Trans.lift decodeEnumAttributeBits
    AttributeTypeExplosion -> AttributeValueExplosion <$> Trans.lift decodeExplosionAttributeBits
    AttributeTypeExtendedExplosion -> AttributeValueExtendedExplosion <$> Trans.lift decodeExtendedExplosionAttributeBits
    AttributeTypeFlaggedInt -> AttributeValueFlaggedInt <$> Trans.lift decodeFlaggedIntAttributeBits
    AttributeTypeFloat -> AttributeValueFloat <$> Trans.lift decodeFloatAttributeBits
    AttributeTypeGameMode -> AttributeValueGameMode <$> Reader.withReaderT fst decodeGameModeAttributeBits
    AttributeTypeInt -> AttributeValueInt <$> Trans.lift decodeIntAttributeBits
    AttributeTypeLoadout -> AttributeValueLoadout <$> Trans.lift decodeLoadoutAttributeBits
    AttributeTypeLoadoutOnline -> AttributeValueLoadoutOnline <$> decodeLoadoutOnlineAttributeBits
    AttributeTypeLoadouts -> AttributeValueLoadouts <$> Trans.lift decodeLoadoutsAttributeBits
    AttributeTypeLoadoutsOnline -> AttributeValueLoadoutsOnline <$> decodeLoadoutsOnlineAttributeBits
    AttributeTypeLocation -> AttributeValueLocation <$> Trans.lift decodeLocationAttributeBits
    AttributeTypeMusicStinger -> AttributeValueMusicStinger <$> Trans.lift decodeMusicStingerAttributeBits
    AttributeTypePartyLeader -> AttributeValuePartyLeader <$> Reader.withReaderT fst decodePartyLeaderAttributeBits
    AttributeTypePickup -> AttributeValuePickup <$> Trans.lift decodePickupAttributeBits
    AttributeTypePrivateMatchSettings -> AttributeValuePrivateMatchSettings <$> Trans.lift decodePrivateMatchSettingsAttributeBits
    AttributeTypeQWord -> AttributeValueQWord <$> Trans.lift decodeQWordAttributeBits
    AttributeTypeReservation -> AttributeValueReservation <$> Reader.withReaderT fst decodeReservationAttributeBits
    AttributeTypeRigidBodyState -> AttributeValueRigidBodyState <$> Trans.lift decodeRigidBodyStateAttributeBits
    AttributeTypeString -> AttributeValueString <$> Trans.lift decodeStringAttributeBits
    AttributeTypeTeamPaint -> AttributeValueTeamPaint <$> Trans.lift decodeTeamPaintAttributeBits
    AttributeTypeUniqueId -> AttributeValueUniqueId <$> Reader.withReaderT fst decodeUniqueIdAttributeBits
    AttributeTypeWeldedInfo -> AttributeValueWeldedInfo <$> Trans.lift decodeWeldedInfoAttributeBits

attributeTypes :: Map Str AttributeType
attributeTypes = Map.mapKeys toStr (Map.fromList rawAttributeTypes)
