module Rattletrap.Encode.AttributeValue
  ( putAttributeValue
  ) where

import Rattletrap.Encode.AppliedDamageAttribute
import Rattletrap.Encode.BooleanAttribute
import Rattletrap.Encode.ByteAttribute
import Rattletrap.Encode.CamSettingsAttribute
import Rattletrap.Encode.ClubColorsAttribute
import Rattletrap.Encode.DamageStateAttribute
import Rattletrap.Encode.DemolishAttribute
import Rattletrap.Encode.EnumAttribute
import Rattletrap.Encode.ExplosionAttribute
import Rattletrap.Encode.ExtendedExplosionAttribute
import Rattletrap.Encode.FlaggedIntAttribute
import Rattletrap.Encode.FloatAttribute
import Rattletrap.Encode.GameModeAttribute
import Rattletrap.Encode.IntAttribute
import Rattletrap.Encode.LoadoutAttribute
import Rattletrap.Encode.LoadoutOnlineAttribute
import Rattletrap.Encode.LoadoutsAttribute
import Rattletrap.Encode.LoadoutsOnlineAttribute
import Rattletrap.Encode.LocationAttribute
import Rattletrap.Encode.MusicStingerAttribute
import Rattletrap.Encode.PartyLeaderAttribute
import Rattletrap.Encode.PickupAttribute
import Rattletrap.Encode.PlayerHistoryKeyAttribute
import Rattletrap.Encode.PrivateMatchSettingsAttribute
import Rattletrap.Encode.QWordAttribute
import Rattletrap.Encode.ReservationAttribute
import Rattletrap.Encode.RigidBodyStateAttribute
import Rattletrap.Encode.StatEventAttribute
import Rattletrap.Encode.StringAttribute
import Rattletrap.Encode.TeamPaintAttribute
import Rattletrap.Encode.UniqueIdAttribute
import Rattletrap.Encode.WeldedInfoAttribute
import Rattletrap.Type.AttributeValue

import qualified Data.Binary.Bits.Put as BinaryBits

putAttributeValue :: AttributeValue -> BinaryBits.BitPut ()
putAttributeValue value = case value of
  AttributeValueAppliedDamage x -> putAppliedDamageAttribute x
  AttributeValueBoolean x -> putBooleanAttribute x
  AttributeValueByte x -> putByteAttribute x
  AttributeValueCamSettings x -> putCamSettingsAttribute x
  AttributeValueClubColors x -> putClubColorsAttribute x
  AttributeValueDamageState x -> putDamageStateAttribute x
  AttributeValueDemolish x -> putDemolishAttribute x
  AttributeValueEnum x -> putEnumAttribute x
  AttributeValueExplosion x -> putExplosionAttribute x
  AttributeValueExtendedExplosion x -> putExtendedExplosionAttribute x
  AttributeValueFlaggedInt x -> putFlaggedIntAttribute x
  AttributeValueFloat x -> putFloatAttribute x
  AttributeValueGameMode x -> putGameModeAttribute x
  AttributeValueInt x -> putIntAttribute x
  AttributeValueLoadout x -> putLoadoutAttribute x
  AttributeValueLoadoutOnline x -> putLoadoutOnlineAttribute x
  AttributeValueLoadouts x -> putLoadoutsAttribute x
  AttributeValueLoadoutsOnline x -> putLoadoutsOnlineAttribute x
  AttributeValueLocation x -> putLocationAttribute x
  AttributeValueMusicStinger x -> putMusicStingerAttribute x
  AttributeValuePartyLeader x -> putPartyLeaderAttribute x
  AttributeValuePickup x -> putPickupAttribute x
  AttributeValuePlayerHistoryKey x -> putPlayerHistoryKeyAttribute x
  AttributeValuePrivateMatchSettings x -> putPrivateMatchSettingsAttribute x
  AttributeValueQWord x -> putQWordAttribute x
  AttributeValueReservation x -> putReservationAttribute x
  AttributeValueRigidBodyState x -> putRigidBodyStateAttribute x
  AttributeValueStatEvent x -> putStatEventAttribute x
  AttributeValueString x -> putStringAttribute x
  AttributeValueTeamPaint x -> putTeamPaintAttribute x
  AttributeValueUniqueId x -> putUniqueIdAttribute x
  AttributeValueWeldedInfo x -> putWeldedInfoAttribute x
