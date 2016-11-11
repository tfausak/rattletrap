module Rattletrap.AttributeValue
  ( module Rattletrap.AttributeValue
  , module Rattletrap.Attribute.Boolean
  , module Rattletrap.Attribute.Byte
  , module Rattletrap.Attribute.CamSettings
  , module Rattletrap.Attribute.ClubColors
  , module Rattletrap.Attribute.Demolish
  , module Rattletrap.Attribute.Enum
  , module Rattletrap.Attribute.Explosion
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

import Rattletrap.Attribute.Boolean
import Rattletrap.Attribute.Byte
import Rattletrap.Attribute.CamSettings
import Rattletrap.Attribute.ClubColors
import Rattletrap.Attribute.Demolish
import Rattletrap.Attribute.Enum
import Rattletrap.Attribute.Explosion
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
import Rattletrap.AttributeValueType
import Rattletrap.Data
import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Map as Map

data AttributeValue
  = BooleanAttributeValue BooleanAttribute
  | ByteAttributeValue ByteAttribute
  | CamSettingsAttributeValue CamSettingsAttribute
  | ClubColorsAttributeValue ClubColorsAttribute
  | DemolishAttributeValue DemolishAttribute
  | EnumAttributeValue EnumAttribute
  | ExplosionAttributeValue ExplosionAttribute
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

getAttributeValue :: (Int, Int) -> Text -> BinaryBit.BitGet AttributeValue
getAttributeValue version name =
  case Map.lookup name attributeValueTypes of
    Just constructor ->
      case constructor of
        AVBoolean -> do
          x <- getBooleanAttribute
          pure (BooleanAttributeValue x)
        AVByte -> do
          x <- getByteAttribute
          pure (ByteAttributeValue x)
        AVCamSettings -> do
          x <- getCamSettingsAttribute
          pure (CamSettingsAttributeValue x)
        AVClubColors -> do
          x <- getClubColorsAttribute
          pure (ClubColorsAttributeValue x)
        AVDemolish -> do
          x <- getDemolishAttribute
          pure (DemolishAttributeValue x)
        AVEnum -> do
          x <- getEnumAttribute
          pure (EnumAttributeValue x)
        AVExplosion -> do
          x <- getExplosionAttribute
          pure (ExplosionAttributeValue x)
        AVFlaggedInt -> do
          x <- getFlaggedIntAttribute
          pure (FlaggedIntAttributeValue x)
        AVFloat -> do
          x <- getFloatAttribute
          pure (FloatAttributeValue x)
        AVGameMode -> do
          x <- getGameModeAttribute version
          pure (GameModeAttributeValue x)
        AVInt -> do
          x <- getIntAttribute
          pure (IntAttributeValue x)
        AVLoadout -> do
          x <- getLoadoutAttribute
          pure (LoadoutAttributeValue x)
        AVLoadoutOnline -> do
          x <- getLoadoutOnlineAttribute
          pure (LoadoutOnlineAttributeValue x)
        AVLoadouts -> do
          x <- getLoadoutsAttribute
          pure (LoadoutsAttributeValue x)
        AVLoadoutsOnline -> do
          x <- getLoadoutsOnlineAttribute
          pure (LoadoutsOnlineAttributeValue x)
        AVLocation -> do
          x <- getLocationAttribute
          pure (LocationAttributeValue x)
        AVMusicStinger -> do
          x <- getMusicStingerAttribute
          pure (MusicStingerAttributeValue x)
        AVPartyLeader -> do
          x <- getPartyLeaderAttribute
          pure (PartyLeaderAttributeValue x)
        AVPickup -> do
          x <- getPickupAttribute
          pure (PickupAttributeValue x)
        AVPrivateMatchSettings -> do
          x <- getPrivateMatchSettingsAttribute
          pure (PrivateMatchSettingsAttributeValue x)
        AVQWord -> do
          x <- getQWordAttribute
          pure (QWordAttributeValue x)
        AVReservation -> do
          x <- getReservationAttribute version
          pure (ReservationAttributeValue x)
        AVRigidBodyState -> do
          x <- getRigidBodyStateAttribute
          pure (RigidBodyStateAttributeValue x)
        AVString -> do
          x <- getStringAttribute
          pure (StringAttributeValue x)
        AVTeamPaint -> do
          x <- getTeamPaintAttribute
          pure (TeamPaintAttributeValue x)
        AVUniqueId -> do
          x <- getUniqueIdAttribute
          pure (UniqueIdAttributeValue x)
        AVWeldedInfo -> do
          x <- getWeldedInfoAttribute
          pure (WeldedInfoAttributeValue x)
    Nothing -> fail ("don't know how to get attribute value " ++ show name)

attributeValueTypes :: Map.Map Text AttributeValueType
attributeValueTypes =
  Map.mapKeys stringToText (Map.fromList rawAttributeValueTypes)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
    BooleanAttributeValue x -> putBooleanAttribute x
    ByteAttributeValue x -> putByteAttribute x
    CamSettingsAttributeValue x -> putCamSettingsAttribute x
    ClubColorsAttributeValue x -> putClubColorsAttribute x
    DemolishAttributeValue x -> putDemolishAttribute x
    EnumAttributeValue x -> putEnumAttribute x
    ExplosionAttributeValue x -> putExplosionAttribute x
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
