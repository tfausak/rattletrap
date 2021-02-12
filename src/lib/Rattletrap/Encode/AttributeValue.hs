module Rattletrap.Encode.AttributeValue
  ( putAttributeValue
  ) where

import Rattletrap.Encode.AppliedDamageAttribute
import Rattletrap.Encode.BooleanAttribute
import Rattletrap.Encode.ByteAttribute
import Rattletrap.Encode.CamSettingsAttribute
import Rattletrap.Encode.ClubColorsAttribute
import Rattletrap.Encode.CustomDemolishAttribute
import Rattletrap.Encode.DamageStateAttribute
import Rattletrap.Encode.DemolishAttribute
import Rattletrap.Encode.EnumAttribute
import Rattletrap.Encode.ExplosionAttribute
import Rattletrap.Encode.ExtendedExplosionAttribute
import Rattletrap.Encode.FlaggedByteAttribute
import Rattletrap.Encode.FlaggedIntAttribute
import Rattletrap.Encode.FloatAttribute
import Rattletrap.Encode.GameModeAttribute
import Rattletrap.Type.Int64Attribute
import Rattletrap.Type.IntAttribute
import Rattletrap.Type.LoadoutAttribute
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Type.LoadoutsAttribute
import Rattletrap.Type.LoadoutsOnlineAttribute
import Rattletrap.Type.LocationAttribute
import Rattletrap.Type.MusicStingerAttribute
import Rattletrap.Type.PartyLeaderAttribute
import Rattletrap.Type.PickupAttribute
import Rattletrap.Type.PickupAttributeNew
import Rattletrap.Type.PlayerHistoryKeyAttribute
import Rattletrap.Type.PrivateMatchSettingsAttribute
import Rattletrap.Type.QWordAttribute
import Rattletrap.Type.ReservationAttribute
import Rattletrap.Type.RigidBodyStateAttribute
import Rattletrap.Type.StatEventAttribute
import Rattletrap.Type.StringAttribute
import Rattletrap.Type.TeamPaintAttribute
import Rattletrap.Type.TitleAttribute
import Rattletrap.Type.UniqueIdAttribute
import Rattletrap.Type.WeldedInfoAttribute
import Rattletrap.Type.AttributeValue

import qualified Data.Binary.Bits.Put as BinaryBits

putAttributeValue :: AttributeValue -> BinaryBits.BitPut ()
putAttributeValue value = case value of
  AttributeValueAppliedDamage x -> putAppliedDamageAttribute x
  AttributeValueBoolean x -> putBooleanAttribute x
  AttributeValueByte x -> putByteAttribute x
  AttributeValueCamSettings x -> putCamSettingsAttribute x
  AttributeValueClubColors x -> putClubColorsAttribute x
  AttributeValueCustomDemolish x -> putCustomDemolishAttribute x
  AttributeValueDamageState x -> putDamageStateAttribute x
  AttributeValueDemolish x -> putDemolishAttribute x
  AttributeValueEnum x -> putEnumAttribute x
  AttributeValueExplosion x -> putExplosionAttribute x
  AttributeValueExtendedExplosion x -> putExtendedExplosionAttribute x
  AttributeValueFlaggedInt x -> putFlaggedIntAttribute x
  AttributeValueFlaggedByte x -> putFlaggedByteAttribute x
  AttributeValueFloat x -> putFloatAttribute x
  AttributeValueGameMode x -> putGameModeAttribute x
  AttributeValueInt x -> putIntAttribute x
  AttributeValueInt64 x -> putInt64Attribute x
  AttributeValueLoadout x -> putLoadoutAttribute x
  AttributeValueLoadoutOnline x -> putLoadoutOnlineAttribute x
  AttributeValueLoadouts x -> putLoadoutsAttribute x
  AttributeValueLoadoutsOnline x -> putLoadoutsOnlineAttribute x
  AttributeValueLocation x -> putLocationAttribute x
  AttributeValueMusicStinger x -> putMusicStingerAttribute x
  AttributeValuePartyLeader x -> putPartyLeaderAttribute x
  AttributeValuePickup x -> putPickupAttribute x
  AttributeValuePickupNew x -> putPickupAttributeNew x
  AttributeValuePlayerHistoryKey x -> putPlayerHistoryKeyAttribute x
  AttributeValuePrivateMatchSettings x -> putPrivateMatchSettingsAttribute x
  AttributeValueQWord x -> putQWordAttribute x
  AttributeValueReservation x -> putReservationAttribute x
  AttributeValueRigidBodyState x -> putRigidBodyStateAttribute x
  AttributeValueStatEvent x -> putStatEventAttribute x
  AttributeValueString x -> putStringAttribute x
  AttributeValueTeamPaint x -> putTeamPaintAttribute x
  AttributeValueTitle x -> putTitleAttribute x
  AttributeValueUniqueId x -> putUniqueIdAttribute x
  AttributeValueWeldedInfo x -> putWeldedInfoAttribute x
