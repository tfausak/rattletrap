module Rattletrap.AttributeValue where

import Rattletrap.Type.AppliedDamageAttribute
import Rattletrap.Decode.AppliedDamageAttribute
import Rattletrap.Encode.AppliedDamageAttribute
import Rattletrap.Type.BooleanAttribute
import Rattletrap.Decode.BooleanAttribute
import Rattletrap.Encode.BooleanAttribute
import Rattletrap.Type.ByteAttribute
import Rattletrap.Decode.ByteAttribute
import Rattletrap.Encode.ByteAttribute
import Rattletrap.Type.CamSettingsAttribute
import Rattletrap.Decode.CamSettingsAttribute
import Rattletrap.Encode.CamSettingsAttribute
import Rattletrap.Type.ClubColorsAttribute
import Rattletrap.Decode.ClubColorsAttribute
import Rattletrap.Encode.ClubColorsAttribute
import Rattletrap.Type.DamageStateAttribute
import Rattletrap.Decode.DamageStateAttribute
import Rattletrap.Encode.DamageStateAttribute
import Rattletrap.Type.DemolishAttribute
import Rattletrap.Decode.DemolishAttribute
import Rattletrap.Encode.DemolishAttribute
import Rattletrap.Type.EnumAttribute
import Rattletrap.Decode.EnumAttribute
import Rattletrap.Encode.EnumAttribute
import Rattletrap.Type.ExplosionAttribute
import Rattletrap.Decode.ExplosionAttribute
import Rattletrap.Encode.ExplosionAttribute
import Rattletrap.Type.ExtendedExplosionAttribute
import Rattletrap.Decode.ExtendedExplosionAttribute
import Rattletrap.Encode.ExtendedExplosionAttribute
import Rattletrap.Type.FlaggedIntAttribute
import Rattletrap.Decode.FlaggedIntAttribute
import Rattletrap.Encode.FlaggedIntAttribute
import Rattletrap.Type.FloatAttribute
import Rattletrap.Decode.FloatAttribute
import Rattletrap.Encode.FloatAttribute
import Rattletrap.Type.GameModeAttribute
import Rattletrap.Decode.GameModeAttribute
import Rattletrap.Encode.GameModeAttribute
import Rattletrap.Type.IntAttribute
import Rattletrap.Decode.IntAttribute
import Rattletrap.Encode.IntAttribute
import Rattletrap.Type.LoadoutAttribute
import Rattletrap.Decode.LoadoutAttribute
import Rattletrap.Encode.LoadoutAttribute
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Decode.LoadoutOnlineAttribute
import Rattletrap.Encode.LoadoutOnlineAttribute
import Rattletrap.Type.LoadoutsAttribute
import Rattletrap.Decode.LoadoutsAttribute
import Rattletrap.Encode.LoadoutsAttribute
import Rattletrap.Type.LoadoutsOnlineAttribute
import Rattletrap.Decode.LoadoutsOnlineAttribute
import Rattletrap.Encode.LoadoutsOnlineAttribute
import Rattletrap.Type.LocationAttribute
import Rattletrap.Decode.LocationAttribute
import Rattletrap.Encode.LocationAttribute
import Rattletrap.Type.MusicStingerAttribute
import Rattletrap.Decode.MusicStingerAttribute
import Rattletrap.Encode.MusicStingerAttribute
import Rattletrap.Type.PartyLeaderAttribute
import Rattletrap.Decode.PartyLeaderAttribute
import Rattletrap.Encode.PartyLeaderAttribute
import Rattletrap.Type.PickupAttribute
import Rattletrap.Decode.PickupAttribute
import Rattletrap.Encode.PickupAttribute
import Rattletrap.Type.PrivateMatchSettingsAttribute
import Rattletrap.Decode.PrivateMatchSettingsAttribute
import Rattletrap.Encode.PrivateMatchSettingsAttribute
import Rattletrap.Type.QWordAttribute
import Rattletrap.Decode.QWordAttribute
import Rattletrap.Encode.QWordAttribute
import Rattletrap.Type.ReservationAttribute
import Rattletrap.Decode.ReservationAttribute
import Rattletrap.Encode.ReservationAttribute
import Rattletrap.Type.RigidBodyStateAttribute
import Rattletrap.Decode.RigidBodyStateAttribute
import Rattletrap.Encode.RigidBodyStateAttribute
import Rattletrap.Type.StringAttribute
import Rattletrap.Decode.StringAttribute
import Rattletrap.Encode.StringAttribute
import Rattletrap.Type.TeamPaintAttribute
import Rattletrap.Decode.TeamPaintAttribute
import Rattletrap.Encode.TeamPaintAttribute
import Rattletrap.Type.UniqueIdAttribute
import Rattletrap.Decode.UniqueIdAttribute
import Rattletrap.Encode.UniqueIdAttribute
import Rattletrap.Type.WeldedInfoAttribute
import Rattletrap.Decode.WeldedInfoAttribute
import Rattletrap.Encode.WeldedInfoAttribute
import Rattletrap.Type.AttributeType
import Rattletrap.Data
import Rattletrap.Type.Word32
import Rattletrap.Type.Text

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Map.Strict as Map

data AttributeValue
  = AppliedDamageAttributeValue AppliedDamageAttribute
  | BooleanAttributeValue BooleanAttribute
  | ByteAttributeValue ByteAttribute
  | CamSettingsAttributeValue CamSettingsAttribute
  | ClubColorsAttributeValue ClubColorsAttribute
  | DamageStateAttributeValue DamageStateAttribute
  | DemolishAttributeValue DemolishAttribute
  | EnumAttributeValue EnumAttribute
  | ExplosionAttributeValue ExplosionAttribute
  | ExtendedExplosionAttributeValue ExtendedExplosionAttribute
  | FlaggedIntAttributeValue FlaggedIntAttribute
  | FloatAttributeValue FloatAttribute
  | GameModeAttributeValue GameModeAttribute
  | IntAttributeValue IntAttribute
  | LoadoutAttributeValue LoadoutAttribute
  | LoadoutOnlineAttributeValue LoadoutOnlineAttribute
  | LoadoutsAttributeValue LoadoutsAttribute
  | LoadoutsOnlineAttributeValue LoadoutsOnlineAttribute
  | LocationAttributeValue LocationAttribute
  | MusicStingerAttributeValue MusicStingerAttribute
  | PartyLeaderAttributeValue PartyLeaderAttribute
  | PickupAttributeValue PickupAttribute
  | PrivateMatchSettingsAttributeValue PrivateMatchSettingsAttribute
  | QWordAttributeValue QWordAttribute
  | ReservationAttributeValue ReservationAttribute
  | RigidBodyStateAttributeValue RigidBodyStateAttribute
  | StringAttributeValue StringAttribute
  | TeamPaintAttributeValue TeamPaintAttribute
  | UniqueIdAttributeValue UniqueIdAttribute
  | WeldedInfoAttributeValue WeldedInfoAttribute
  deriving (Eq, Ord, Show)

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

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value = case value of
  AppliedDamageAttributeValue x -> putAppliedDamageAttribute x
  BooleanAttributeValue x -> putBooleanAttribute x
  ByteAttributeValue x -> putByteAttribute x
  CamSettingsAttributeValue x -> putCamSettingsAttribute x
  ClubColorsAttributeValue x -> putClubColorsAttribute x
  DamageStateAttributeValue x -> putDamageStateAttribute x
  DemolishAttributeValue x -> putDemolishAttribute x
  EnumAttributeValue x -> putEnumAttribute x
  ExplosionAttributeValue x -> putExplosionAttribute x
  ExtendedExplosionAttributeValue x -> putExtendedExplosionAttribute x
  FlaggedIntAttributeValue x -> putFlaggedIntAttribute x
  FloatAttributeValue x -> putFloatAttribute x
  GameModeAttributeValue x -> putGameModeAttribute x
  IntAttributeValue x -> putIntAttribute x
  LoadoutAttributeValue x -> putLoadoutAttribute x
  LoadoutOnlineAttributeValue x -> putLoadoutOnlineAttribute x
  LoadoutsAttributeValue x -> putLoadoutsAttribute x
  LoadoutsOnlineAttributeValue x -> putLoadoutsOnlineAttribute x
  LocationAttributeValue x -> putLocationAttribute x
  MusicStingerAttributeValue x -> putMusicStingerAttribute x
  PartyLeaderAttributeValue x -> putPartyLeaderAttribute x
  PickupAttributeValue x -> putPickupAttribute x
  PrivateMatchSettingsAttributeValue x -> putPrivateMatchSettingsAttribute x
  QWordAttributeValue x -> putQWordAttribute x
  ReservationAttributeValue x -> putReservationAttribute x
  RigidBodyStateAttributeValue x -> putRigidBodyStateAttribute x
  StringAttributeValue x -> putStringAttribute x
  TeamPaintAttributeValue x -> putTeamPaintAttribute x
  UniqueIdAttributeValue x -> putUniqueIdAttribute x
  WeldedInfoAttributeValue x -> putWeldedInfoAttribute x
