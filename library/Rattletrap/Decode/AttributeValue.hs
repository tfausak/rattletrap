module Rattletrap.Decode.AttributeValue
  ( getAttributeValue
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Type.AttributeValue
import Rattletrap.Type.AttributeType
import Rattletrap.Decode.AppliedDamageAttribute
import Rattletrap.Decode.BooleanAttribute
import Rattletrap.Decode.ByteAttribute
import Rattletrap.Decode.CamSettingsAttribute
import Rattletrap.Decode.ClubColorsAttribute
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
import Rattletrap.Data
import Rattletrap.Type.Word32le
import Rattletrap.Type.Str

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getAttributeValue
  :: (Int, Int, Int)
  -> Map.Map Word32le Str
  -> Str
  -> BinaryBit.BitGet AttributeValue
getAttributeValue version objectMap name =
  case Map.lookup name attributeTypes of
    Just constructor -> case constructor of
      AttributeTypeAppliedDamage -> do
        x <- getAppliedDamageAttribute
        pure (AttributeValueAppliedDamage x)
      AttributeTypeBoolean -> do
        x <- getBooleanAttribute
        pure (AttributeValueBoolean x)
      AttributeTypeByte -> do
        x <- getByteAttribute
        pure (AttributeValueByte x)
      AttributeTypeCamSettings -> do
        x <- getCamSettingsAttribute version
        pure (AttributeValueCamSettings x)
      AttributeTypeClubColors -> do
        x <- getClubColorsAttribute
        pure (AttributeValueClubColors x)
      AttributeTypeDamageState -> do
        x <- getDamageStateAttribute
        pure (AttributeValueDamageState x)
      AttributeTypeDemolish -> do
        x <- getDemolishAttribute
        pure (AttributeValueDemolish x)
      AttributeTypeEnum -> do
        x <- getEnumAttribute
        pure (AttributeValueEnum x)
      AttributeTypeExplosion -> do
        x <- getExplosionAttribute
        pure (AttributeValueExplosion x)
      AttributeTypeExtendedExplosion -> do
        x <- getExtendedExplosionAttribute
        pure (AttributeValueExtendedExplosion x)
      AttributeTypeFlaggedInt -> do
        x <- getFlaggedIntAttribute
        pure (AttributeValueFlaggedInt x)
      AttributeTypeFloat -> do
        x <- getFloatAttribute
        pure (AttributeValueFloat x)
      AttributeTypeGameMode -> do
        x <- getGameModeAttribute version
        pure (AttributeValueGameMode x)
      AttributeTypeInt -> do
        x <- getIntAttribute
        pure (AttributeValueInt x)
      AttributeTypeLoadout -> do
        x <- getLoadoutAttribute
        pure (AttributeValueLoadout x)
      AttributeTypeLoadoutOnline -> do
        x <- getLoadoutOnlineAttribute version objectMap
        pure (AttributeValueLoadoutOnline x)
      AttributeTypeLoadouts -> do
        x <- getLoadoutsAttribute
        pure (AttributeValueLoadouts x)
      AttributeTypeLoadoutsOnline -> do
        x <- getLoadoutsOnlineAttribute version objectMap
        pure (AttributeValueLoadoutsOnline x)
      AttributeTypeLocation -> do
        x <- getLocationAttribute
        pure (AttributeValueLocation x)
      AttributeTypeMusicStinger -> do
        x <- getMusicStingerAttribute
        pure (AttributeValueMusicStinger x)
      AttributeTypePartyLeader -> do
        x <- getPartyLeaderAttribute version
        pure (AttributeValuePartyLeader x)
      AttributeTypePickup -> do
        x <- getPickupAttribute
        pure (AttributeValuePickup x)
      AttributeTypePrivateMatchSettings -> do
        x <- getPrivateMatchSettingsAttribute
        pure (AttributeValuePrivateMatchSettings x)
      AttributeTypeQWord -> do
        x <- getQWordAttribute
        pure (AttributeValueQWord x)
      AttributeTypeReservation -> do
        x <- getReservationAttribute version
        pure (AttributeValueReservation x)
      AttributeTypeRigidBodyState -> do
        x <- getRigidBodyStateAttribute
        pure (AttributeValueRigidBodyState x)
      AttributeTypeString -> do
        x <- getStringAttribute
        pure (AttributeValueString x)
      AttributeTypeTeamPaint -> do
        x <- getTeamPaintAttribute
        pure (AttributeValueTeamPaint x)
      AttributeTypeUniqueId -> do
        x <- getUniqueIdAttribute version
        pure (AttributeValueUniqueId x)
      AttributeTypeWeldedInfo -> do
        x <- getWeldedInfoAttribute
        pure (AttributeValueWeldedInfo x)
    Nothing -> fail ("don't know how to get attribute value " <> show name)

attributeTypes :: Map.Map Str AttributeType
attributeTypes = Map.mapKeys toStr (Map.fromList rawAttributeTypes)
