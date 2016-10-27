module Rattletrap.AttributeValue
  ( module Rattletrap.AttributeValue
  , module Export
  ) where

import Rattletrap.AttributeValue.Boolean as Export
import Rattletrap.AttributeValue.Byte as Export
import Rattletrap.AttributeValue.CamSettings as Export
import Rattletrap.AttributeValue.Demolish as Export
import Rattletrap.AttributeValue.Enum as Export
import Rattletrap.AttributeValue.Explosion as Export
import Rattletrap.AttributeValue.FlaggedInt as Export
import Rattletrap.AttributeValue.Float as Export
import Rattletrap.AttributeValue.GameMode as Export
import Rattletrap.AttributeValue.Int as Export
import Rattletrap.AttributeValue.Loadout as Export
import Rattletrap.AttributeValue.LoadoutOnline as Export
import Rattletrap.AttributeValue.Loadouts as Export
import Rattletrap.AttributeValue.LoadoutsOnline as Export
import Rattletrap.AttributeValue.Location as Export
import Rattletrap.AttributeValue.MusicStinger as Export
import Rattletrap.AttributeValue.PartyLeader as Export
import Rattletrap.AttributeValue.Pickup as Export
import Rattletrap.AttributeValue.PrivateMatchSettings as Export
import Rattletrap.AttributeValue.QWord as Export
import Rattletrap.AttributeValue.Reservation as Export
import Rattletrap.AttributeValue.RigidBodyState as Export
import Rattletrap.AttributeValue.String as Export
import Rattletrap.AttributeValue.TeamPaint as Export
import Rattletrap.AttributeValue.UniqueId as Export
import Rattletrap.AttributeValue.WeldedInfo as Export

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
