module Rattletrap.Decode.AttributeValue
  ( getAttributeValue
  ) where

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
import Rattletrap.Type.Word32
import Rattletrap.Type.Text

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map.Strict as Map

getAttributeValue
  :: (Int, Int, Int)
  -> Map.Map Word32 Text
  -> Text
  -> BinaryBit.BitGet AttributeValue
getAttributeValue version objectMap name =
  case Map.lookup name attributeTypes of
    Just constructor -> case constructor of
      AppliedDamageAttributeType -> do
        x <- getAppliedDamageAttribute
        pure (AppliedDamageAttributeValue x)
      BooleanAttributeType -> do
        x <- getBooleanAttribute
        pure (BooleanAttributeValue x)
      ByteAttributeType -> do
        x <- getByteAttribute
        pure (ByteAttributeValue x)
      CamSettingsAttributeType -> do
        x <- getCamSettingsAttribute version
        pure (CamSettingsAttributeValue x)
      ClubColorsAttributeType -> do
        x <- getClubColorsAttribute
        pure (ClubColorsAttributeValue x)
      DamageStateAttributeType -> do
        x <- getDamageStateAttribute
        pure (DamageStateAttributeValue x)
      DemolishAttributeType -> do
        x <- getDemolishAttribute
        pure (DemolishAttributeValue x)
      EnumAttributeType -> do
        x <- getEnumAttribute
        pure (EnumAttributeValue x)
      ExplosionAttributeType -> do
        x <- getExplosionAttribute
        pure (ExplosionAttributeValue x)
      ExtendedExplosionAttributeType -> do
        x <- getExtendedExplosionAttribute
        pure (ExtendedExplosionAttributeValue x)
      FlaggedIntAttributeType -> do
        x <- getFlaggedIntAttribute
        pure (FlaggedIntAttributeValue x)
      FloatAttributeType -> do
        x <- getFloatAttribute
        pure (FloatAttributeValue x)
      GameModeAttributeType -> do
        x <- getGameModeAttribute version
        pure (GameModeAttributeValue x)
      IntAttributeType -> do
        x <- getIntAttribute
        pure (IntAttributeValue x)
      LoadoutAttributeType -> do
        x <- getLoadoutAttribute
        pure (LoadoutAttributeValue x)
      LoadoutOnlineAttributeType -> do
        x <- getLoadoutOnlineAttribute version objectMap
        pure (LoadoutOnlineAttributeValue x)
      LoadoutsAttributeType -> do
        x <- getLoadoutsAttribute
        pure (LoadoutsAttributeValue x)
      LoadoutsOnlineAttributeType -> do
        x <- getLoadoutsOnlineAttribute version objectMap
        pure (LoadoutsOnlineAttributeValue x)
      LocationAttributeType -> do
        x <- getLocationAttribute
        pure (LocationAttributeValue x)
      MusicStingerAttributeType -> do
        x <- getMusicStingerAttribute
        pure (MusicStingerAttributeValue x)
      PartyLeaderAttributeType -> do
        x <- getPartyLeaderAttribute version
        pure (PartyLeaderAttributeValue x)
      PickupAttributeType -> do
        x <- getPickupAttribute
        pure (PickupAttributeValue x)
      PrivateMatchSettingsAttributeType -> do
        x <- getPrivateMatchSettingsAttribute
        pure (PrivateMatchSettingsAttributeValue x)
      QWordAttributeType -> do
        x <- getQWordAttribute
        pure (QWordAttributeValue x)
      ReservationAttributeType -> do
        x <- getReservationAttribute version
        pure (ReservationAttributeValue x)
      RigidBodyStateAttributeType -> do
        x <- getRigidBodyStateAttribute
        pure (RigidBodyStateAttributeValue x)
      StringAttributeType -> do
        x <- getStringAttribute
        pure (StringAttributeValue x)
      TeamPaintAttributeType -> do
        x <- getTeamPaintAttribute
        pure (TeamPaintAttributeValue x)
      UniqueIdAttributeType -> do
        x <- getUniqueIdAttribute version
        pure (UniqueIdAttributeValue x)
      WeldedInfoAttributeType -> do
        x <- getWeldedInfoAttribute
        pure (WeldedInfoAttributeValue x)
    Nothing -> fail ("don't know how to get attribute value " ++ show name)

attributeTypes :: Map.Map Text AttributeType
attributeTypes = Map.mapKeys stringToText (Map.fromList rawAttributeTypes)
