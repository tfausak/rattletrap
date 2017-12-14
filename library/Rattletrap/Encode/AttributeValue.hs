module Rattletrap.Encode.AttributeValue
  ( putAttributeValue
  ) where

import Rattletrap.Type.AttributeValue
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
import Rattletrap.Encode.PrivateMatchSettingsAttribute
import Rattletrap.Encode.QWordAttribute
import Rattletrap.Encode.ReservationAttribute
import Rattletrap.Encode.RigidBodyStateAttribute
import Rattletrap.Encode.StringAttribute
import Rattletrap.Encode.TeamPaintAttribute
import Rattletrap.Encode.UniqueIdAttribute
import Rattletrap.Encode.WeldedInfoAttribute

import qualified Data.Binary.Bits.Put as BinaryBit

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
