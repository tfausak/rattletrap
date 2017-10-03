module Rattletrap.AttributeValue
  ( module Rattletrap.AttributeValue
  , module Rattletrap.Attribute.AppliedDamage
  , module Rattletrap.Attribute.Boolean
  , module Rattletrap.Attribute.Byte
  , module Rattletrap.Attribute.CamSettings
  , module Rattletrap.Attribute.ClubColors
  , module Rattletrap.Attribute.DamageState
  , module Rattletrap.Attribute.Demolish
  , module Rattletrap.Attribute.Enum
  , module Rattletrap.Attribute.Explosion
  , module Rattletrap.Attribute.ExtendedExplosion
  , module Rattletrap.Attribute.FlaggedInt
  , module Rattletrap.Attribute.Float
  , module Rattletrap.Attribute.GameMode
  , module Rattletrap.Attribute.Int
  , module Rattletrap.Attribute.Loadout
  , module Rattletrap.Attribute.LoadoutOnline
  , module Rattletrap.Attribute.Loadouts
  , module Rattletrap.Attribute.LoadoutsOnline
  , module Rattletrap.Attribute.Location
  , module Rattletrap.Attribute.MusicStinger
  , module Rattletrap.Attribute.PartyLeader
  , module Rattletrap.Attribute.Pickup
  , module Rattletrap.Attribute.PrivateMatchSettings
  , module Rattletrap.Attribute.QWord
  , module Rattletrap.Attribute.Reservation
  , module Rattletrap.Attribute.RigidBodyState
  , module Rattletrap.Attribute.String
  , module Rattletrap.Attribute.TeamPaint
  , module Rattletrap.Attribute.UniqueId
  , module Rattletrap.Attribute.WeldedInfo
  ) where

import Rattletrap.Attribute.AppliedDamage
import Rattletrap.Attribute.Boolean
import Rattletrap.Attribute.Byte
import Rattletrap.Attribute.CamSettings
import Rattletrap.Attribute.ClubColors
import Rattletrap.Attribute.DamageState
import Rattletrap.Attribute.Demolish
import Rattletrap.Attribute.Enum
import Rattletrap.Attribute.Explosion
import Rattletrap.Attribute.ExtendedExplosion
import Rattletrap.Attribute.FlaggedInt
import Rattletrap.Attribute.Float
import Rattletrap.Attribute.GameMode
import Rattletrap.Attribute.Int
import Rattletrap.Attribute.Loadout
import Rattletrap.Attribute.LoadoutOnline
import Rattletrap.Attribute.Loadouts
import Rattletrap.Attribute.LoadoutsOnline
import Rattletrap.Attribute.Location
import Rattletrap.Attribute.MusicStinger
import Rattletrap.Attribute.PartyLeader
import Rattletrap.Attribute.Pickup
import Rattletrap.Attribute.PrivateMatchSettings
import Rattletrap.Attribute.QWord
import Rattletrap.Attribute.Reservation
import Rattletrap.Attribute.RigidBodyState
import Rattletrap.Attribute.String
import Rattletrap.Attribute.TeamPaint
import Rattletrap.Attribute.UniqueId
import Rattletrap.Attribute.WeldedInfo
import Rattletrap.AttributeType
import Rattletrap.Data
import Rattletrap.Primitive

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

getAttributeValue :: (Int, Int) -> Map.Map Word32 Text -> Text -> BinaryBit.BitGet AttributeValue
getAttributeValue version objectMap name =
  case Map.lookup name attributeTypes of
    Just constructor ->
      case constructor of
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
          x <- getPartyLeaderAttribute
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
          x <- getUniqueIdAttribute
          pure (UniqueIdAttributeValue x)
        WeldedInfoAttributeType -> do
          x <- getWeldedInfoAttribute
          pure (WeldedInfoAttributeValue x)
    Nothing -> fail ("don't know how to get attribute value " ++ show name)

attributeTypes :: Map.Map Text AttributeType
attributeTypes = Map.mapKeys stringToText (Map.fromList rawAttributeTypes)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
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
