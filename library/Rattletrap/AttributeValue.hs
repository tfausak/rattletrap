module Rattletrap.AttributeValue
  ( module Rattletrap.AttributeValue
  , module Rattletrap.AttributeValue.Boolean
  , module Rattletrap.AttributeValue.Byte
  , module Rattletrap.AttributeValue.CamSettings
  , module Rattletrap.AttributeValue.ClubColors
  , module Rattletrap.AttributeValue.Demolish
  , module Rattletrap.AttributeValue.Enum
  , module Rattletrap.AttributeValue.Explosion
  , module Rattletrap.AttributeValue.FlaggedInt
  , module Rattletrap.AttributeValue.Float
  , module Rattletrap.AttributeValue.GameMode
  , module Rattletrap.AttributeValue.Int
  , module Rattletrap.AttributeValue.Loadout
  , module Rattletrap.AttributeValue.LoadoutOnline
  , module Rattletrap.AttributeValue.Loadouts
  , module Rattletrap.AttributeValue.LoadoutsOnline
  , module Rattletrap.AttributeValue.Location
  , module Rattletrap.AttributeValue.MusicStinger
  , module Rattletrap.AttributeValue.PartyLeader
  , module Rattletrap.AttributeValue.Pickup
  , module Rattletrap.AttributeValue.PrivateMatchSettings
  , module Rattletrap.AttributeValue.QWord
  , module Rattletrap.AttributeValue.Reservation
  , module Rattletrap.AttributeValue.RigidBodyState
  , module Rattletrap.AttributeValue.String
  , module Rattletrap.AttributeValue.TeamPaint
  , module Rattletrap.AttributeValue.UniqueId
  , module Rattletrap.AttributeValue.WeldedInfo
  ) where

import Rattletrap.AttributeValue.Boolean
import Rattletrap.AttributeValue.Byte
import Rattletrap.AttributeValue.CamSettings
import Rattletrap.AttributeValue.ClubColors
import Rattletrap.AttributeValue.Demolish
import Rattletrap.AttributeValue.Enum
import Rattletrap.AttributeValue.Explosion
import Rattletrap.AttributeValue.FlaggedInt
import Rattletrap.AttributeValue.Float
import Rattletrap.AttributeValue.GameMode
import Rattletrap.AttributeValue.Int
import Rattletrap.AttributeValue.Loadout
import Rattletrap.AttributeValue.LoadoutOnline
import Rattletrap.AttributeValue.Loadouts
import Rattletrap.AttributeValue.LoadoutsOnline
import Rattletrap.AttributeValue.Location
import Rattletrap.AttributeValue.MusicStinger
import Rattletrap.AttributeValue.PartyLeader
import Rattletrap.AttributeValue.Pickup
import Rattletrap.AttributeValue.PrivateMatchSettings
import Rattletrap.AttributeValue.QWord
import Rattletrap.AttributeValue.Reservation
import Rattletrap.AttributeValue.RigidBodyState
import Rattletrap.AttributeValue.String
import Rattletrap.AttributeValue.TeamPaint
import Rattletrap.AttributeValue.UniqueId
import Rattletrap.AttributeValue.WeldedInfo
import Rattletrap.AttributeValueType
import Rattletrap.Data
import Rattletrap.Text

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Map as Map

data AttributeValue
  = BooleanAttribute BooleanAttributeValue
  | ByteAttribute ByteAttributeValue
  | CamSettingsAttribute CamSettingsAttributeValue
  | ClubColorsAttribute ClubColorsAttributeValue
  | DemolishAttribute DemolishAttributeValue
  | EnumAttribute EnumAttributeValue
  | ExplosionAttribute ExplosionAttributeValue
  | FlaggedIntAttribute FlaggedIntAttributeValue
  | FloatAttribute FloatAttributeValue
  | GameModeAttribute GameModeAttributeValue
  | IntAttribute IntAttributeValue
  | LoadoutAttribute LoadoutAttributeValue
  | LoadoutOnlineAttribute LoadoutOnlineAttributeValue
  | LoadoutsAttribute LoadoutsAttributeValue
  | LoadoutsOnlineAttribute LoadoutsOnlineAttributeValue
  | LocationAttribute LocationAttributeValue
  | MusicStingerAttribute MusicStingerAttributeValue
  | PartyLeaderAttribute PartyLeaderAttributeValue
  | PickupAttribute PickupAttributeValue
  | PrivateMatchSettingsAttribute PrivateMatchSettingsAttributeValue
  | QWordAttribute QWordAttributeValue
  | ReservationAttribute ReservationAttributeValue
  | RigidBodyStateAttribute RigidBodyStateAttributeValue
  | StringAttribute StringAttributeValue
  | TeamPaintAttribute TeamPaintAttributeValue
  | UniqueIdAttribute UniqueIdAttributeValue
  | WeldedInfoAttribute WeldedInfoAttributeValue
  deriving (Eq, Ord, Show)

getAttributeValue :: (Int, Int) -> Text -> BinaryBit.BitGet AttributeValue
getAttributeValue version name =
  case Map.lookup name attributeValueTypes of
    Just constructor ->
      case constructor of
        AVBoolean -> do
          x <- getBooleanAttributeValue
          pure (BooleanAttribute x)
        AVByte -> do
          x <- getByteAttributeValue
          pure (ByteAttribute x)
        AVCamSettings -> do
          x <- getCamSettingsAttributeValue
          pure (CamSettingsAttribute x)
        AVClubColors -> do
          x <- getClubColorsAttributeValue
          pure (ClubColorsAttribute x)
        AVDemolish -> do
          x <- getDemolishAttributeValue
          pure (DemolishAttribute x)
        AVEnum -> do
          x <- getEnumAttributeValue
          pure (EnumAttribute x)
        AVExplosion -> do
          x <- getExplosionAttributeValue
          pure (ExplosionAttribute x)
        AVFlaggedInt -> do
          x <- getFlaggedIntAttributeValue
          pure (FlaggedIntAttribute x)
        AVFloat -> do
          x <- getFloatAttributeValue
          pure (FloatAttribute x)
        AVGameMode -> do
          x <- getGameModeAttributeValue version
          pure (GameModeAttribute x)
        AVInt -> do
          x <- getIntAttributeValue
          pure (IntAttribute x)
        AVLoadout -> do
          x <- getLoadoutAttributeValue
          pure (LoadoutAttribute x)
        AVLoadoutOnline -> do
          x <- getLoadoutOnlineAttributeValue
          pure (LoadoutOnlineAttribute x)
        AVLoadouts -> do
          x <- getLoadoutsAttributeValue
          pure (LoadoutsAttribute x)
        AVLoadoutsOnline -> do
          x <- getLoadoutsOnlineAttributeValue
          pure (LoadoutsOnlineAttribute x)
        AVLocation -> do
          x <- getLocationAttributeValue
          pure (LocationAttribute x)
        AVMusicStinger -> do
          x <- getMusicStingerAttributeValue
          pure (MusicStingerAttribute x)
        AVPartyLeader -> do
          x <- getPartyLeaderAttributeValue
          pure (PartyLeaderAttribute x)
        AVPickup -> do
          x <- getPickupAttributeValue
          pure (PickupAttribute x)
        AVPrivateMatchSettings -> do
          x <- getPrivateMatchSettingsAttributeValue
          pure (PrivateMatchSettingsAttribute x)
        AVQWord -> do
          x <- getQWordAttributeValue
          pure (QWordAttribute x)
        AVReservation -> do
          x <- getReservationAttributeValue version
          pure (ReservationAttribute x)
        AVRigidBodyState -> do
          x <- getRigidBodyStateAttributeValue
          pure (RigidBodyStateAttribute x)
        AVString -> do
          x <- getStringAttributeValue
          pure (StringAttribute x)
        AVTeamPaint -> do
          x <- getTeamPaintAttributeValue
          pure (TeamPaintAttribute x)
        AVUniqueId -> do
          x <- getUniqueIdAttributeValue
          pure (UniqueIdAttribute x)
        AVWeldedInfo -> do
          x <- getWeldedInfoAttributeValue
          pure (WeldedInfoAttribute x)
    Nothing -> fail ("don't know how to get attribute value " ++ show name)

attributeValueTypes :: Map.Map Text AttributeValueType
attributeValueTypes =
  Map.mapKeys stringToText (Map.fromList rawAttributeValueTypes)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
    BooleanAttribute x -> putBooleanAttributeValue x
    ByteAttribute x -> putByteAttributeValue x
    CamSettingsAttribute x -> putCamSettingsAttributeValue x
    ClubColorsAttribute x -> putClubColorsAttributeValue x
    DemolishAttribute x -> putDemolishAttributeValue x
    EnumAttribute x -> putEnumAttributeValue x
    ExplosionAttribute x -> putExplosionAttributeValue x
    FlaggedIntAttribute x -> putFlaggedIntAttributeValue x
    FloatAttribute x -> putFloatAttributeValue x
    GameModeAttribute x -> putGameModeAttributeValue x
    IntAttribute x -> putIntAttributeValue x
    LoadoutAttribute x -> putLoadoutAttributeValue x
    LoadoutOnlineAttribute x -> putLoadoutOnlineAttributeValue x
    LoadoutsAttribute x -> putLoadoutsAttributeValue x
    LoadoutsOnlineAttribute x -> putLoadoutsOnlineAttributeValue x
    LocationAttribute x -> putLocationAttributeValue x
    MusicStingerAttribute x -> putMusicStingerAttributeValue x
    PartyLeaderAttribute x -> putPartyLeaderAttributeValue x
    PickupAttribute x -> putPickupAttributeValue x
    PrivateMatchSettingsAttribute x -> putPrivateMatchSettingsAttributeValue x
    QWordAttribute x -> putQWordAttributeValue x
    ReservationAttribute x -> putReservationAttributeValue x
    RigidBodyStateAttribute x -> putRigidBodyStateAttributeValue x
    StringAttribute x -> putStringAttributeValue x
    TeamPaintAttribute x -> putTeamPaintAttributeValue x
    UniqueIdAttribute x -> putUniqueIdAttributeValue x
    WeldedInfoAttribute x -> putWeldedInfoAttributeValue x
